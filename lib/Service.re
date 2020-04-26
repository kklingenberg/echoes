open Lwt.Infix;
open Httpaf_lwt_unix;

module Log = Dolog.Log;

let server = (addr, port) =>
  Lwt_io.establish_server_with_client_socket(
    Unix.(ADDR_INET(inet_addr_of_string(addr), port)),
    Server.create_connection_handler(
      ~request_handler=Api.make_request_handler(Endpoints.routes),
      ~error_handler=Api.error_handler,
    ),
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
