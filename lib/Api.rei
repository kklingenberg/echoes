module Response: {
  type t;
  let body: t => string;
  let status: t => int;
  let raw: (~status: int=?, string) => t;
  let json: (~status: int=?, Yojson.Safe.t) => t;
  let error: (~status: int=?, list(string)) => t;
};

module Route: {
  type handler = (Httpaf.Reqd.t, list(string)) => Lwt.t(Response.t);
  type t;
  let get: (string, handler) => t;
  let head: (string, handler) => t;
  let post: (string, handler) => t;
  let put: (string, handler) => t;
  let delete: (string, handler) => t;
};

let make_request_handler:
  (list(Route.t), Unix.sockaddr) => Httpaf.Server_connection.request_handler;
let error_handler: Unix.sockaddr => Httpaf.Server_connection.error_handler;
