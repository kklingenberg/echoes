/* A response wrapper, may be used by handlers to create responses. */
module Response: {
  type t;
  let body: t => string;
  let headers: t => list((string, string));
  let status: t => int;
  let raw: (~status: int=?, ~headers: list((string, string))=?, string) => t;
  let json: (~status: int=?, ~headers: list((string, string))=?, Yojson.Safe.t) => t;
  let error: (~status: int=?, ~headers: list((string, string))=?, list(string)) => t;
};

/* A route handler mechanism, whereby a handler is a procedure that
   produces a response given a request and the parsed URL parameters
   from it. */
module Route: {
  type handler = (Httpaf.Reqd.t, list(string)) => Lwt.t(Response.t);
  type t;
  let get: (string, handler) => list(t);
  let post: (string, handler) => list(t);
  let put: (string, handler) => list(t);
  let delete: (string, handler) => list(t);
};

/* Make a request handler for the httpaf server, given a set of
   handlers */
let make_request_handler:
  (list(list(Route.t)), Unix.sockaddr) => Httpaf.Server_connection.request_handler;

/* The error handler for the httpaf server */
let error_handler: Unix.sockaddr => Httpaf.Server_connection.error_handler;
