open Utils;

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
          ("idempotencyKey", `Stringlit(IdempotencyKey.to_string(m.idempotency_key))),
          ("destination", `Stringlit(Actor.to_raw(m.destination))),
        ])
        |> to_string
      | ActorMessage(m) =>
        `Assoc([
          ("type", `Stringlit("msg")),
          ("idempotencyKey", `Stringlit(IdempotencyKey.to_string(m.idempotency_key))),
          ("origin", `Stringlit(Actor.to_raw(m.origin))),
          ("body", m.body),
        ])
        |> to_string
      }
    );

let pull_string = (items, field) =>
  switch (List.assoc(field, items)) {
  | `Stringlit(s) => Ok(s)
  | exception Not_found => Error([Printf.sprintf("field %s not found", field)])
  | _ => Error([Printf.sprintf("invalid %s field: not a string literal", field)])
  };

let pull_stringlist = (items, field) =>
  switch (List.assoc(field, items)) {
  | `List(subitems) =>
    List.mapi(
      (index, v) =>
        switch (v) {
        | `Stringlit(s) => Ok(s)
        | _ =>
          Error([
            Printf.sprintf(
              "invalid %s field: element #%d is not a string literal",
              field,
              index,
            ),
          ])
        },
      subitems,
    )
    |> join_results
  | exception Not_found => Error([Printf.sprintf("field %s not found", field)])
  | _ => Error([Printf.sprintf("invalid %s field: it is not a list", field)])
  };

let pull_variant = (items, field) =>
  switch (List.assoc(field, items)) {
  | exception Not_found => Error([Printf.sprintf("field %s not found", field)])
  | v => Ok(v)
  };

let deserialize_stored_message: (Actor.t, string) => result(message, list(string)) =
  (actor, serialized) => {
    open Yojson.Raw;

    let deserialize_stored_ack = items =>
      join2(
        pull_string(items, "idempotencyKey") >>= IdempotencyKey.from_string,
        pull_string(items, "destination") >>= Actor.from_raw,
      )
      >>= (((k, d)) => Ok(Ack({idempotency_key: k, origin: actor, destination: d})));

    let deserialize_stored_msg = items =>
      join3(
        pull_string(items, "idempotencyKey") >>= IdempotencyKey.from_string,
        pull_string(items, "origin") >>= Actor.from_raw,
        pull_variant(items, "body"),
      )
      >>= (
        ((k, o, b)) =>
          Ok(
            ActorMessage({idempotency_key: k, origin: o, destination: actor, body: b}),
          )
      );

    switch (from_string(serialized)) {
    | `Assoc(items) =>
      switch (List.assoc("type", items)) {
      | `Stringlit("ack") => deserialize_stored_ack(items)
      | `Stringlit("msg") => deserialize_stored_msg(items)
      | exception Not_found => Error(["missing stored message type"])
      | `Stringlit(t) => Error([Printf.sprintf("invalid stored message type: %s", t)])
      | _ => Error(["invalid stored message type (not a string literal)"])
      }
    | exception _ => Error(["invalid stored message (invalid JSON)"])
    | _ => Error(["invalid stored message (not a JSON object)"])
    };
  };

let deserialize_enveloped_message:
  (Actor.t, string) => result(list(message), list(string)) =
  (actor, serialized) =>
    Yojson.Raw.(
      switch (from_string(serialized)) {
      | `Assoc(items) =>
        join3(
          pull_string(items, "idempotencyKey") >>= IdempotencyKey.from_string,
          pull_stringlist(items, "destination")
          >>= join_results
          % List.map(Actor.from_raw),
          pull_variant(items, "body"),
        )
        >>= (
          ((k, ds, b)) =>
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
      | exception _ => Error(["Invalid enveloped message (invalid JSON)"])
      | _ => Error(["Invalid enveloped message (not a JSON object)"])
      }
    );
