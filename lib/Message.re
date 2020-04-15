/* Monadic bind for result */
let (>>=): (result('a, 't), 'a => result('b, 't)) => result('b, 't) =
  (o, f) =>
    switch (o) {
    | Ok(v) => f(v)
    | Error(_) as error => error
    };

/* Reduce errors */
let join_results: list(result('a, 't)) => result(list('a), 't) =
  items =>
    List.fold_right(
      (e, acc) =>
        switch (e, acc) {
        | (Ok(elem), Ok(accum)) => Ok([elem, ...accum])
        | (Error(_) as error, Ok(_)) => error
        | (_, Error(_) as error) => error
        },
      items,
      Ok([]),
    );

module type IdempotencyKeyType = {
  type t;
  let min_length: int;
  let max_length: int;
  let is_safe_char: char => bool;
  let from_string: string => result(t, string);
  let to_string: t => string;
};

module IdempotencyKey: IdempotencyKeyType = {
  type t =
    | Safe(string);

  let min_length = 12;
  let max_length = 64;

  module CharSet = Set.Make(Char);
  let base64_alphabet: CharSet.t = {
    let letters = "abcdefghijklmnopqrstuvwxyz";
    letters
    ++ String.uppercase_ascii(letters)
    ++ "0123456789-_="
    |> String.to_seq
    |> List.of_seq
    |> CharSet.of_list;
  };
  let is_safe_char = c => CharSet.mem(c, base64_alphabet);

  let from_string = raw =>
    if (String.length(raw) < min_length) {
      Error("idempotency key is too short");
    } else if (String.length(raw) > max_length) {
      Error("idempotency key is too long");
    } else if (raw
               |> String.to_seq
               |> List.of_seq
               |> List.for_all(is_safe_char)) {
      Error("idempotency key has invalid characters");
    } else {
      Ok(Safe(raw));
    };

  let to_string = (Safe(s)) => s;
};

module type ActorType = {
  type t;
  let from_raw: string => result(t, string);
  let from_encoded: string => result(t, string);
  let to_raw: t => string;
  let to_encoded: t => string;
};

module Actor: ActorType = {
  type t =
    | Encoded(string, string);

  let from_raw = s =>
    switch (s) {
    | "" => Error("empty raw string for actor type")
    | _ =>
      Ok(
        Encoded(
          s,
          Netencoding.Base64.encode(s)
          |> Str.global_replace(Str.regexp("\\+"), "-")
          |> Str.global_replace(Str.regexp("/"), "_"),
        ),
      )
    };

  let from_encoded = s =>
    switch (
      s
      |> Str.global_replace(Str.regexp("_"), "/")
      |> Str.global_replace(Str.regexp("-"), "+")
      |> Netencoding.Base64.decode
    ) {
    | raw when String.length(raw) > 0 => Ok(Encoded(raw, s))
    | exception _ => Error("invalid encoded string for actor type")
    | _ => Error("empty encoded string for actor type")
    };

  let to_raw = (Encoded(raw, _)) => raw;
  let to_encoded = (Encoded(_, encoded)) => encoded;
};

type ack = {
  idempotency_key: IdempotencyKey.t,
  origin: Actor.t,
  destination: Actor.t,
};

type message_body = Yojson.Raw.t;

type actor_message = {
  idempotency_key: IdempotencyKey.t,
  origin: Actor.t,
  destination: Actor.t,
  body: message_body,
};

type message =
  | Ack(ack)
  | ActorMessage(actor_message);

type t = message;

let actor_key_lookup: Actor.t => string =
  actor =>
    Printf.sprintf(
      "%s%s:*",
      Settings.current^.redis_key_prefix,
      Actor.to_encoded(actor),
    );

let message_key: message => string =
  msg =>
    switch (msg) {
    | Ack(m) =>
      Printf.sprintf(
        "%s%s:%s:%s:ack",
        Settings.current^.redis_key_prefix,
        Actor.to_encoded(m.origin),
        Actor.to_encoded(m.destination),
        IdempotencyKey.to_string(m.idempotency_key),
      )
    | ActorMessage(m) =>
      Printf.sprintf(
        "%s%s:%s:%s",
        Settings.current^.redis_key_prefix,
        Actor.to_encoded(m.destination),
        Actor.to_encoded(m.origin),
        IdempotencyKey.to_string(m.idempotency_key),
      )
    };

let serialize_message: message => string =
  msg =>
    Yojson.Raw.(
      switch (msg) {
      | Ack(m) =>
        `Assoc([
          ("type", `Stringlit("ack")),
          (
            "idempotencyKey",
            `Stringlit(IdempotencyKey.to_string(m.idempotency_key)),
          ),
          ("destination", `Stringlit(Actor.to_raw(m.destination))),
        ])
        |> to_string
      | ActorMessage(m) =>
        `Assoc([
          ("type", `Stringlit("msg")),
          (
            "idempotencyKey",
            `Stringlit(IdempotencyKey.to_string(m.idempotency_key)),
          ),
          ("origin", `Stringlit(Actor.to_raw(m.origin))),
          ("body", m.body),
        ])
        |> to_string
      }
    );

let deserialize_string_field = (items, field, mapfn) =>
  switch (List.assoc(field, items)) {
  | `Stringlit(s) => mapfn(s)
  | exception Not_found => Error(Printf.sprintf("field %s not found", field))
  | _ =>
    Error(Printf.sprintf("invalid %s field: not a string literal", field))
  };

let deserialize_stringlist_field = (items, field, mapfn) =>
  switch (List.assoc(field, items)) {
  | `List(subitems) =>
    List.mapi(
      (index, v) =>
        switch (v) {
        | `Stringlit(s) => mapfn(s)
        | _ =>
          Error(
            Printf.sprintf(
              "invalid %s field: element #%d is not a string literal",
              field,
              index,
            ),
          )
        },
      subitems,
    )
    |> join_results
  | exception Not_found => Error(Printf.sprintf("field %s not found", field))
  | _ => Error(Printf.sprintf("invalid %s field: it is not a list", field))
  };

let deserialize_variant = (items, field) =>
  switch (List.assoc(field, items)) {
  | exception Not_found => Error(Printf.sprintf("field %s not found", field))
  | v => Ok(v)
  };

let deserialize_stored_message: (Actor.t, string) => result(message, string) =
  (actor, serialized) => {
    open Yojson.Raw;

    let deserialize_stored_ack = items =>
      deserialize_string_field(
        items,
        "idempotencyKey",
        IdempotencyKey.from_string,
      )
      >>= (
        k =>
          deserialize_string_field(items, "destination", Actor.from_raw)
          >>= (
            d => Ok(Ack({idempotency_key: k, origin: actor, destination: d}))
          )
      );

    let deserialize_stored_msg = items =>
      deserialize_string_field(
        items,
        "idempotencyKey",
        IdempotencyKey.from_string,
      )
      >>= (
        k =>
          deserialize_string_field(items, "origin", Actor.from_raw)
          >>= (
            o =>
              deserialize_variant(items, "body")
              >>= (
                b =>
                  Ok(
                    ActorMessage({
                      idempotency_key: k,
                      origin: o,
                      destination: actor,
                      body: b,
                    }),
                  )
              )
          )
      );

    switch (from_string(serialized)) {
    | `Assoc(items) =>
      switch (List.assoc("type", items)) {
      | `Stringlit("ack") => deserialize_stored_ack(items)
      | `Stringlit("msg") => deserialize_stored_msg(items)
      | exception Not_found => Error("missing stored message type")
      | `Stringlit(t) =>
        Error(Printf.sprintf("invalid stored message type: %s", t))
      | _ => Error("invalid stored message type (not a string literal)")
      }
    | exception _ => Error("invalid stored message (invalid JSON)")
    | _ => Error("invalid stored message (not a JSON object)")
    };
  };

let deserialize_enveloped_message:
  (Actor.t, string) => result(list(message), string) =
  (actor, serialized) =>
    Yojson.Raw.(
      switch (from_string(serialized)) {
      | `Assoc(items) =>
        deserialize_string_field(
          items,
          "idempotencyKey",
          IdempotencyKey.from_string,
        )
        >>= (
          k =>
            deserialize_stringlist_field(items, "destination", Actor.from_raw)
            >>= (
              ds =>
                deserialize_variant(items, "body")
                >>= (
                  b =>
                    Ok(
                      List.map(
                        d =>
                          ActorMessage({
                            idempotency_key: k,
                            origin: actor,
                            destination: d,
                            body: b,
                          }),
                        ds,
                      ),
                    )
                )
            )
        )
      | exception _ => Error("Invalid enveloped message (invalid JSON)")
      | _ => Error("Invalid enveloped message (not a JSON object)")
      }
    );
