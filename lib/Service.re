open Lwt.Infix;
open Httpaf;
open Httpaf_lwt_unix;

module Log = Dolog.Log;

let request_handler = (_, reqd) =>
  Reqd.respond_with_string(
    reqd,
    Response.create(~headers=Headers.of_list([("Connection", "close")]), `OK),
    "Hellow",
  );

let error_handler = (_, ~request as _=?, error, start_response) => {
  let response_body = start_response(Headers.empty);
  switch (error) {
  | `Exn(_) =>
    Body.write_string(response_body, "Exception happened; oops");
    Body.write_string(response_body, "\n");
  | _ => Body.write_string(response_body, "Something else happened")
  };
  Body.close_writer(response_body);
};

let server = (addr, port) =>
  Lwt_io.establish_server_with_client_socket(
    Unix.(ADDR_INET(inet_addr_of_string(addr), port)),
    Server.create_connection_handler(~request_handler, ~error_handler),
  )
  >>= (
    srv => {
      Log.info("Started echoes server listening to %s:%d", addr, port);
      let (cycle, finish) = Lwt.task();
      let _ =
        Lwt_unix.on_signal(
          Sys.sigint,
          _ => {
            Log.info("%s", "Stopping echoes server");
            Lwt.wakeup_later(finish, ());
          },
        );
      let _ =
        Lwt_unix.on_signal(
          Sys.sigterm,
          _ => {
            Log.info("%s", "Stopping echoes server");
            Lwt.wakeup_later(finish, ());
          },
        );
      cycle >>= (() => Lwt_io.shutdown_server(srv));
    }
  );
