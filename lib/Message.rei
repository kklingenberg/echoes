type t;

/* Redis key utils */
let message_key: t => string;

/* Unique serializer for messages */
let serialize_message: t => string;

/* One deserializer for each source: reids and envelope */
let deserialize_stored_message: (Actor.t, string) => result(t, list(string));
let deserialize_enveloped_message: (Actor.t, string) => result(list(t), list(string));
