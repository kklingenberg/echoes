open Utils;
open Utils.Result;

type ack = {
  idempotency_key: IdempotencyKey.t,
  origin: Actor.t,
  destination: Actor.t,
  received_at: string,
};

type message_body = Yojson.Safe.t;

type actor_message = {
  idempotency_key: IdempotencyKey.t,
  origin: Actor.t,
  destination: Actor.t,
  body: message_body,
  expects_ack: bool,
  sent_at: string,
};

type message =
  | Ack(ack)
  | ActorMessage(actor_message);

type t = message;

let acks_from_many = msgs =>
  msgs
  |> List.filter(msg =>
       switch (msg) {
       | Ack(_) => false
       | ActorMessage({expects_ack, _}) => expects_ack
       }
     )
  |> List.map(msg =>
       switch (msg) {
       | Ack(_) => msg
       | ActorMessage(m) =>
         Ack({
           idempotency_key: m.idempotency_key,
           origin: m.origin,
           destination: m.destination,
           received_at: string_of_now(),
         })
       }
     );

let storage_key: message => string =
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

let serialize: message => Yojson.Safe.t =
  msg =>
    switch (msg) {
    | Ack(m) =>
      `Assoc([
        ("type", `String("ack")),
        ("idempotencyKey", `String(IdempotencyKey.to_string(m.idempotency_key))),
        ("destination", `String(Actor.to_raw(m.destination))),
        ("receivedAt", `String(m.received_at)),
      ])
    | ActorMessage(m) =>
      `Assoc([
        ("type", `String("msg")),
        ("idempotencyKey", `String(IdempotencyKey.to_string(m.idempotency_key))),
        ("origin", `String(Actor.to_raw(m.origin))),
        ("body", m.body),
        ("expectsAck", `Bool(m.expects_ack)),
        ("sentAt", `String(m.sent_at)),
      ])
    };

let serialize_to_string: message => string =
  msg => Yojson.Safe.to_string(serialize(msg));

let pull_string = (items, field) =>
  switch (List.assoc(field, items)) {
  | `String(s) => Ok(s)
  | exception Not_found => Error([Printf.sprintf("field %s not found", field)])
  | _ => Error([Printf.sprintf("invalid %s field: not a string literal", field)])
  };

let pull_stringlist = (items, field) =>
  switch (List.assoc(field, items)) {
  | `List(subitems) =>
    List.mapi(
      (index, v) =>
        switch (v) {
        | `String(s) => Ok(s)
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
    |> join_list
  | exception Not_found => Error([Printf.sprintf("field %s not found", field)])
  | _ => Error([Printf.sprintf("invalid %s field: it is not a list", field)])
  };

let pull_boolean = (items, field) =>
  switch (List.assoc(field, items)) {
  | `Bool(v) => Ok(v)
  | exception Not_found => Ok(false)
  | _ => Error([Printf.sprintf("invalid %s field: it is not a boolean value", field)])
  };

let pull_variant = (items, field) =>
  switch (List.assoc(field, items)) {
  | exception Not_found => Error([Printf.sprintf("field %s not found", field)])
  | v => Ok(v)
  };

let deserialize_stored: (Actor.t, Yojson.Safe.t) => result(message, list(string)) =
  (actor, serialized) => {
    let deserialize_stored_ack = items =>
      join3(
        pull_string(items, "idempotencyKey") >>= IdempotencyKey.from_string,
        pull_string(items, "destination") >>= Actor.from_raw,
        pull_string(items, "receivedAt"),
      )
      >>= (
        ((k, d, r)) =>
          Ok(Ack({idempotency_key: k, origin: actor, destination: d, received_at: r}))
      );

    let deserialize_stored_msg = items =>
      join5(
        pull_string(items, "idempotencyKey") >>= IdempotencyKey.from_string,
        pull_string(items, "origin") >>= Actor.from_raw,
        pull_variant(items, "body"),
        pull_boolean(items, "expectsAck"),
        pull_string(items, "sentAt"),
      )
      >>= (
        ((k, o, b, e, s)) =>
          Ok(
            ActorMessage({
              idempotency_key: k,
              origin: o,
              destination: actor,
              body: b,
              expects_ack: e,
              sent_at: s,
            }),
          )
      );

    switch (serialized) {
    | `Assoc(items) =>
      switch (List.assoc("type", items)) {
      | `String("ack") => deserialize_stored_ack(items)
      | `String("msg") => deserialize_stored_msg(items)
      | exception Not_found => Error(["missing stored message type"])
      | `String(t) => Error([Printf.sprintf("invalid stored message type: %s", t)])
      | _ => Error(["invalid stored message type (not a string literal)"])
      }

    | _ => Error(["invalid stored message (not a JSON object)"])
    };
  };

let deserialize_stored_from_string: (Actor.t, string) => result(message, list(string)) =
  (actor, serialized) =>
    switch (Yojson.Safe.from_string(serialized)) {
    | exception _ => Error(["invalid stored message (invalid JSON)"])
    | obj => deserialize_stored(actor, obj)
    };

let deserialize_enveloped:
  (Actor.t, Yojson.Safe.t) => result(list(message), list(string)) =
  (actor, serialized) =>
    switch (serialized) {
    | `Assoc(items) =>
      join4(
        pull_string(items, "idempotencyKey") >>= IdempotencyKey.from_string,
        pull_stringlist(items, "destination") >>= join_list % List.map(Actor.from_raw),
        pull_variant(items, "body"),
        pull_boolean(items, "expectsAck"),
      )
      >>= (
        ((k, ds, b, e)) => {
          let now = string_of_now();
          Ok(
            List.map(
              d =>
                ActorMessage({
                  idempotency_key: k,
                  origin: actor,
                  destination: d,
                  body: b,
                  expects_ack: e,
                  sent_at: now,
                }),
              ds,
            ),
          );
        }
      )
    | _ => Error(["Invalid enveloped message (not a JSON object)"])
    };

let deserialize_enveloped_from_string:
  (Actor.t, string) => result(list(message), list(string)) =
  (actor, serialized) =>
    switch (Yojson.Safe.from_string(serialized)) {
    | exception _ => Error(["Invalid enveloped message (invalid JSON)"])
    | obj => deserialize_enveloped(actor, obj)
    };
