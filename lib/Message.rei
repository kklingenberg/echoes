type t;

let acks_from_messages: list(t) => list(t);

/* Redis key utils */
let message_key: t => string;

/* Unique serializer for messages */
let serialize_message: t => Yojson.Safe.t;
let serialize_message_to_string: t => string;

/* One deserializer for each source: redis and envelope */
let deserialize_stored_message: (Actor.t, string) => result(t, list(string));
let deserialize_enveloped_message: (Actor.t, string) => result(list(t), list(string));
