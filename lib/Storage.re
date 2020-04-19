open Lwt.Infix;

module Log = Dolog.Log;
module C = Redis_lwt.Client;

let get_connection: unit => C.connection_spec =
  () => {host: Settings.current^.redis_host, port: Settings.current^.redis_port};

let save_messages: list(Message.t) => Lwt.t(unit) =
  messages =>
    C.with_connection(get_connection(), conn =>
      List.fold_left(
        (p, msg) =>
          p
          >>= (
            () =>
              C.setex(
                conn,
                Message.message_key(msg),
                Settings.current^.redis_expiration_seconds,
                Message.serialize_message(msg),
              )
          ),
        Lwt.return(),
        messages,
      )
    );

let messages_for_actor: Actor.t => Lwt.t(list(Message.t)) =
  actor =>
    C.with_connection(
      get_connection(),
      conn => {
        let pattern = Actor.key_lookup(actor);
        let rec by_page = (cursor, previous_keys) =>
          C.scan(~pattern, conn, cursor)
          >>= (
            page => {
              let (next, page_keys) = page;
              let accumulated_keys = List.concat([page_keys, previous_keys]);
              if (next == 0) {
                Lwt.return(accumulated_keys);
              } else {
                by_page(next, accumulated_keys);
              };
            }
          );
        by_page(0, [])
        >>= (
          keys =>
            C.mget(conn, keys)
            >|= (
              msgs =>
                msgs
                |> List.filter(Option.is_some)
                |> List.map(Option.get)
                |> List.map(Message.deserialize_stored_message(actor))
                |> List.filter(r =>
                     switch (r) {
                     | Ok(_) => true
                     | Error(errors) =>
                       Log.error(
                         "Encountered errors during deserialization of stored message:\n%s",
                         String.concat("\n", errors),
                       );
                       false;
                     }
                   )
                |> List.map(Utils.get_ok)
            )
        );
      },
    );
