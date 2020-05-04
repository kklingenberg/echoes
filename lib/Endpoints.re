open Lwt.Infix;
module EchoesVersion = Version;
open Httpaf;
open Api;

let expect_actor = (fn, reqd, params) => {
  let from = Reqd.request(reqd).headers |> Headers.get(_, "From");
  switch (from) {
  | Some(raw_actor) =>
    switch (Actor.from_raw(raw_actor)) {
    | Ok(actor) => fn(reqd, params, actor)
    | Error(details) => Lwt.return(Response.error(~status=400, details))
    }
  | None => Lwt.return(Response.error(~status=400, ["Missing From header"]))
  };
};

let slurp_request = reqd => {
  let (promise, resolver) = Lwt.wait();
  let request_body = Reqd.request_body(reqd);
  let body = ref([]);
  let rec on_read = (buffer, ~off, ~len) => {
    body := [Bigstringaf.substring(buffer, ~off, ~len), ...body^];
    Body.schedule_read(request_body, ~on_read, ~on_eof);
  }
  and on_eof = () =>
    Lwt.wakeup_later(resolver, body^ |> List.rev |> String.concat(""));
  Body.schedule_read(request_body, ~on_read, ~on_eof);
  promise;
};

let routes: list(Route.t) = [
  /* Show version information. */
  Route.get("/", (_, _) =>
    Lwt.return(
      Response.json(
        `Assoc([
          ("version", `String(EchoesVersion.version)),
          ("sha1", `String(EchoesVersion.sha1)),
          ("build", `String(EchoesVersion.build)),
        ]),
      ),
    )
  ),
  /* Exchange incoming messages with messages destined for the
     actor. */
  Route.post(
    "/exchange",
    expect_actor((reqd, _, actor) =>
      slurp_request(reqd)
      >>= (
        body =>
          switch (Message.deserialize_enveloped_from_string(actor, body)) {
          | Ok(incoming_messages) =>
            Storage.with_connection(conn =>
              Storage.save_messages(incoming_messages, conn)
              >>= (() => Storage.read_outgoing(actor, conn))
              >|= (
                outgoing_messages =>
                  Response.json(`List(List.map(Message.serialize, outgoing_messages)))
              )
            )
          | Error(details) => Lwt.return(Response.error(~status=400, details))
          }
      )
    ),
  ),
  /* Receive incoming messages, but don't attempt to read the messages
     destined for the actor. */
  Route.post(
    "/send",
    expect_actor((reqd, _, actor) =>
      slurp_request(reqd)
      >>= (
        body =>
          switch (Message.deserialize_enveloped_from_string(actor, body)) {
          | Ok(incoming_messages) =>
            Storage.with_connection(conn =>
              Storage.save_messages(incoming_messages, conn)
            )
            >|= (
              () =>
                Response.json(
                  `Assoc([("sent", `Int(List.length(incoming_messages)))]),
                )
            )
          | Error(details) => Lwt.return(Response.error(~status=400, details))
          }
      )
    ),
  ),
  /* Read messages destined for the actor. */
  Route.post(
    "/receive",
    expect_actor((_, _, actor) =>
      Storage.with_connection(conn =>
        Storage.read_outgoing(actor, conn)
        >|= (
          outgoing_messages =>
            Response.json(`List(List.map(Message.serialize, outgoing_messages)))
        )
      )
    ),
  ),
];
