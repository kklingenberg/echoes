type t;

let acks_from_many: list(t) => list(t);

/* Redis key utils */
let storage_key: t => string;

/* Unique serializer for messages */
let serialize: t => Yojson.Safe.t;
let serialize_to_string: t => string;

/* One deserializer for each source: redis and envelope */
let deserialize_stored: (Actor.t, Yojson.Safe.t) => result(t, list(string));
let deserialize_stored_from_string: (Actor.t, string) => result(t, list(string));

let deserialize_enveloped: (Actor.t, Yojson.Safe.t) => result(list(t), list(string));
let deserialize_enveloped_from_string:
  (Actor.t, string) => result(list(t), list(string));
