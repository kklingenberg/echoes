open Lwt.Infix;

module Log = Dolog.Log;
module C = Redis_lwt.Client;

let with_connection: (C.connection => Lwt.t('a)) => Lwt.t('a) =
  f =>
    C.with_connection(
      {host: Settings.current^.redis_host, port: Settings.current^.redis_port},
      f,
    );

let save_messages: (list(Message.t), C.connection) => Lwt.t(unit) =
  (messages, conn) =>
    List.fold_left(
      (p, msg) =>
        p
        >>= (
          () =>
            C.setex(
              conn,
              Message.storage_key(msg),
              Settings.current^.redis_expiration_seconds,
              Message.serialize_to_string(msg),
            )
        ),
      Lwt.return(),
      messages,
    );

module KeySet = Set.Make(String);

let messages_for_actor: (Actor.t, C.connection) => Lwt.t(list((string, Message.t))) =
  (actor, conn) => {
    let pattern = Actor.key_lookup(actor);
    let count = Settings.current^.redis_page_size;
    let rec by_page = (cursor, previous_keys) =>
      C.scan(~pattern, ~count, conn, cursor)
      >>= (
        page => {
          let (next, page_keys) = page;
          let accumulated_keys = KeySet.(union(previous_keys, of_list(page_keys)));
          if (next == 0) {
            Lwt.return(accumulated_keys);
          } else {
            by_page(next, accumulated_keys);
          };
        }
      );
    by_page(0, KeySet.empty)
    >>= (
      scrolled_keys => {
        let keys = KeySet.elements(scrolled_keys);
        List.length(keys) == 0 ?
          Lwt.return([]) :
          C.mget(conn, keys)
          >|= (
            msgs =>
              List.combine(keys, msgs)
              |> List.filter(pair => Option.is_some(snd(pair)))
              |> List.rev_map(pair => (fst(pair), Option.get(snd(pair))))
              |> List.rev_map(pair =>
                   (
                     fst(pair),
                     Message.deserialize_stored_from_string(actor, snd(pair)),
                   )
                 )
              |> List.filter(pair =>
                   switch (pair) {
                   | (_, Ok(_)) => true
                   | (_, Error(errors)) =>
                     Log.error(
                       "Encountered errors during deserialization of stored message:\n%s",
                       String.concat("\n", errors),
                     );
                     false;
                   }
                 )
              |> List.rev_map(pair => (fst(pair), Utils.Result.get_ok(snd(pair))))
          );
      }
    );
  };

let purge_keys: (list(string), C.connection) => Lwt.t(unit) =
  (keys, conn) =>
    List.length(keys) == 0 ? Lwt.return() : C.del(conn, keys) >|= (_ => ());
