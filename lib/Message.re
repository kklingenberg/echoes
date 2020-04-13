module Log = Dolog.Log;

module type IdempotencyKeyType = {
  type t;
  let min_length: int;
  let max_length: int;
  let is_safe_char: char => bool;
  let from_string: string => option(t);
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
    if (String.length(raw) >= min_length
        && String.length(raw) <= max_length
        && raw
        |> String.to_seq
        |> List.of_seq
        |> List.for_all(is_safe_char)) {
      Some(Safe(raw));
    } else {
      None;
    };

  let to_string = (Safe(s)) => s;
};

module type ActorType = {
  type t;
  let from_raw: string => option(t);
  let from_encoded: string => option(t);
  let to_raw: t => string;
  let to_encoded: t => string;
};

module Actor: ActorType = {
  type t =
    | Encoded(string, string);

  let from_raw = s =>
    switch (s) {
    | "" => None
    | _ =>
      Some(
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
    | raw when String.length(raw) > 0 => Some(Encoded(raw, s))
    | exception _
    | _ => None
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
            "idempotency_key",
            `Stringlit(IdempotencyKey.to_string(m.idempotency_key)),
          ),
          ("destination", `Stringlit(Actor.to_raw(m.destination))),
        ])
        |> to_string
      | ActorMessage(m) =>
        `Assoc([
          ("type", `Stringlit("msg")),
          (
            "idempotency_key",
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
  | exception Not_found => None
  | _ => None
  };

let propagate_none: list(option('a)) => option(list('a)) =
  items =>
    List.fold_right(
      (e, acc) =>
        switch (e, acc) {
        | (Some(elem), Some(accum)) => Some([elem, ...accum])
        | _ => None
        },
      items,
      Some([]),
    );

let deserialize_stringlist_field = (items, field, mapfn) =>
  switch (List.assoc(field, items)) {
  | `Stringlit(s) =>
    Str.split(Str.regexp(","), s)
    |> List.map(String.trim)
    |> List.map(mapfn)
    |> propagate_none
  | `List(subitems) =>
    List.map(
      v =>
        switch (v) {
        | `Stringlit(s) => mapfn(s)
        | _ => None
        },
      subitems,
    )
    |> propagate_none
  | exception Not_found => None
  | _ => None
  };

let deserialize_variant = (items, field) =>
  switch (List.assoc(field, items)) {
  | exception Not_found => None
  | v => Some(v)
  };

let deserialize_stored_message: (Actor.t, string) => option(message) =
  (actor, serialized) => {
    open Yojson.Raw;

    let deserialize_stored_ack = items => {
      let idempotency_key =
        deserialize_string_field(
          items,
          "idempotency_key",
          IdempotencyKey.from_string,
        );
      let destination =
        deserialize_string_field(items, "destination", Actor.from_raw);
      switch (idempotency_key, destination) {
      | (Some(k), Some(d)) =>
        Some(Ack({idempotency_key: k, origin: actor, destination: d}))
      | (None, _) =>
        Log.error("Invalid or missing idempotency_key in stored message");
        None;
      | (_, None) =>
        Log.error("Invalid or missing destination in stored message");
        None;
      };
    };

    let deserialize_stored_msg = items => {
      let idempotency_key =
        deserialize_string_field(
          items,
          "idempotency_key",
          IdempotencyKey.from_string,
        );
      let origin = deserialize_string_field(items, "origin", Actor.from_raw);
      let body = deserialize_variant(items, "body");

      switch (idempotency_key, origin, body) {
      | (Some(k), Some(o), Some(b)) =>
        Some(
          ActorMessage({idempotency_key: k, origin: o, destination: actor, body: b}),
        )
      | (None, _, _) =>
        Log.error("Invalid or missing idempotency_key in stored message");
        None;
      | (_, None, _) =>
        Log.error("Invalid or missing origin in stored message");
        None;
      | (_, _, None) =>
        Log.error("Missing body in stored message");
        None;
      };
    };

    switch (from_string(serialized)) {
    | `Assoc(items) =>
      switch (List.assoc("type", items)) {
      | `Stringlit("ack") => deserialize_stored_ack(items)
      | `Stringlit("msg") => deserialize_stored_msg(items)
      | exception Not_found =>
        Log.error("Missing message type");
        None;
      | `Stringlit(t) =>
        Log.error("Invalid stored message type: %s", t);
        None;
      | _ =>
        Log.error("Invalid stored message type (not a string literal)");
        None;
      }
    | exception _ =>
      Log.error("Invalid stored message (invalid JSON)");
      None;
    | _ =>
      Log.error("Invalid stored message (not a JSON object)");
      None;
    };
  };

let deserialize_enveloped_message: (Actor.t, string) => result(list(message), string) =
  (actor, serialized) =>
    Yojson.Raw.(
      switch (from_string(serialized)) {
      | `Assoc(items) =>
        let idempotency_key =
          deserialize_string_field(
            items,
            "idempotency_key",
            IdempotencyKey.from_string,
          );
        let destinations =
          deserialize_stringlist_field(items, "destination", Actor.from_raw);
        let body = deserialize_variant(items, "body");
        switch (idempotency_key, destinations, body) {
        | (Some(k), Some(ds), Some(b)) =>
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
        | (None, _, _) =>
          Error("Invalid or missing idempotency_key in enveloped message")
        | (_, None, _) =>
          Error("Invalid or missing destination in enveloped message")
        | (_, _, None) => Error("Missing body in enveloped message")
        };
      | exception _ => Error("Invalid enveloped message (invalid JSON)")
      | _ => Error("Invalid enveloped message (not a JSON object)")
      }
    );
