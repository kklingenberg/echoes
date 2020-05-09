open Lwt.Infix;
module EchoesVersion = Version;
open Httpaf;

module Log = Dolog.Log;

/* Builds the headers used for a response sending *body* and *extra*
   headers. Sorts them alphabetically. */
let base_headers = (~extra=[], body) =>
  [
    /* Date of the response */
    (
      "Date",
      CalendarLib.Printer.Calendar.sprint(
        "%a, %d %b %Y %H:%M:%S GMT",
        CalendarLib.Calendar.to_gmt(CalendarLib.Calendar.now()),
      ),
    ),
    /* Everything here is JSON, unless an empty response is being sent */
    (
      "Content-Type",
      String.length(body) == 0 ? "text/plain; charset=utf-8" : "application/json",
    ),
    ("Content-Length", string_of_int(String.length(body))),
    /* HTTP 1.1 connection header */
    ("Connection", "keep-alive"),
    /* Server version */
    ("Server", Printf.sprintf("echoes/%s", EchoesVersion.version)),
    /* CORS headers */
    ("Access-Control-Allow-Credentials", "true"),
    ("Access-Control-Allow-Origin", "*"),
    ...extra,
  ]
  |> List.sort(((a, _), (b, _)) => String.compare(a, b))
  |> Headers.of_list;

/* A response wrapper, may be used by handlers to create responses. */
module Response = {
  type t = (string, list((string, string)), int);
  let body = ((b, _, _)) => b;
  let headers = ((_, h, _)) => h;
  let status = ((_, _, s)) => s;
  let raw = (~status: int=200, ~headers: list((string, string))=[], b: string): t => (
    b,
    headers,
    status,
  );
  let json = (~status: int=200, ~headers: list((string, string))=[], b: Yojson.Safe.t) => (
    Yojson.Safe.to_string(b),
    headers,
    status,
  );
  let error =
      (~status: int=500, ~headers: list((string, string))=[], details: list(string)) => (
    Yojson.Safe.(
      to_string(
        `Assoc([
          (
            "error",
            `String(
              switch (status) {
              | 400 => "Bad request"
              | 401 => "Unauthorized"
              | 403 => "Forbidden"
              | 404 => "Not found"
              | 405 => "Method not allowed"
              | 500 => "Internal server error"
              | 501 => "Not implemented"
              | 502 => "Bad gateway"
              | 503 => "Service unavailable"
              | 504 => "Gateway timeout"
              | _ => "Unknown error"
              },
            ),
          ),
          ("status", `Int(status)),
          ("details", `List(List.map(detail => `String(detail), details))),
        ]),
      )
    ),
    headers,
    status,
  );
};

/* Respond with only the headers for a response, without a body. This
   is used for HEAD responses. */
let respond_headers = (reqd: Reqd.t, response: Response.t) =>
  Reqd.respond_with_string(
    reqd,
    Httpaf.Response.create(
      ~headers=
        base_headers(~extra=Response.headers(response), Response.body(response)),
      Status.of_code(Response.status(response)),
    ),
    "",
  );

/* Issue an httpaf response given a wrapped Response type */
let respond = (reqd: Reqd.t, response: Response.t) =>
  Reqd.respond_with_string(
    reqd,
    Httpaf.Response.create(
      ~headers=
        base_headers(~extra=Response.headers(response), Response.body(response)),
      Status.of_code(Response.status(response)),
    ),
    Response.body(response),
  );

/* A route handler mechanism, whereby a handler is a procedure that
   produces a response given a request and the parsed URL parameters
   from it. */
module Route = {
  type handler = (Httpaf.Reqd.t, list(string)) => Lwt.t(Response.t);
  type t = ((Str.regexp, int), Method.t, handler);

  let regexp_of_routepath = p => {
    let parts = Neturl.norm_path(Neturl.split_path(p));
    let groups =
      parts |> List.filter(part => part == ":w" || part == ":p") |> List.length;
    let assembled =
      parts
      |> List.map(part =>
           switch (part) {
           | ":w" => "\\([^/]+\\)"
           | ":p" => "\\(.+\\)"
           | other => Str.quote(other)
           }
         )
      |> Neturl.join_path;
    (
      Str.regexp(
        Printf.sprintf(
          "^%s%s$",
          assembled,
          assembled.[String.length(assembled) - 1] == '/' ? "?" : "/?",
        ),
      ),
      groups,
    );
  };

  /* Add a HEAD route handler for every GET handler */
  let get = (p, h) => [
    (regexp_of_routepath(p), `GET, h),
    (regexp_of_routepath(p), `HEAD, h),
  ];
  let post = (p, h) => [(regexp_of_routepath(p), `POST, h)];
  let put = (p, h) => [(regexp_of_routepath(p), `PUT, h)];
  let delete = (p, h) => [(regexp_of_routepath(p), `DELETE, h)];
};

let path_syntax =
  Hashtbl.find(Neturl.common_url_syntax, "http") |> Neturl.partial_url_syntax;

/* Make a request handler for the httpaf server, given a set of
   handlers */
let make_request_handler = (routes, _, reqd) => {
  let url = Neturl.url_of_string(path_syntax, Reqd.request(reqd).target);
  Log.debug(
    "%s %s",
    Method.to_string(Reqd.request(reqd).meth),
    Neturl.string_of_url(url),
  );
  /* Normalize the path to avoid mismatching */
  let path = Neturl.url_path(url) |> Neturl.norm_path |> Neturl.join_path;
  /* Find those routes matching on path only */
  let matched =
    List.concat(routes)
    |> List.filter_map(route => {
         let ((pattern, groupcount), _method, _handler) = route;
         if (Str.string_match(pattern, path, 0)) {
           let groups = ref([]);
           for (g in 1 to groupcount) {
             groups := [Str.matched_group(g, path), ...groups^];
           };
           Some((route, List.rev(groups^)));
         } else {
           None;
         };
       });
  /* Select those matching the method as well */
  let matched_routes =
    List.filter_map(
      r => {
        let ((_, method, handler), params) = r;
        if (Reqd.request(reqd).meth == method) {
          Some((handler, params));
        } else {
          None;
        };
      },
      matched,
    );
  if (List.length(matched_routes) > 1) {
    Log.warn("Path %s matched by several routes", path);
  };
  /* Keep the first one of those */
  let route = List.nth_opt(matched_routes, 0);
  let allowed_methods =
    matched
    |> List.map((((_, m, _), _)) => Method.to_string(m))
    |> List.sort_uniq(String.compare)
    |> String.concat(", ");
  switch (route) {
  | None when List.length(matched) == 0 =>
    respond(
      reqd,
      Response.error(
        ~status=404,
        [Printf.sprintf("No route matched the given path: %s", path)],
      ),
    )
  /* An OPTIONS response should indicate valid methods */
  | None when Reqd.request(reqd).meth == `OPTIONS =>
    respond(
      reqd,
      Response.raw(
        ~status=204,
        ~headers=[
          ("Allow", allowed_methods),
          ("Access-Control-Allow-Methods", allowed_methods),
        ],
        "",
      ),
    )
  | None =>
    respond(
      reqd,
      Response.error(
        ~status=405,
        ~headers=[("Allow", allowed_methods)],
        [
          Printf.sprintf(
            "A route matched the given path, but the specified method is not appropriate: %s",
            Method.to_string(Reqd.request(reqd).meth),
          ),
        ],
      ),
    )
  | Some((handler, params)) =>
    Lwt.async(() =>
      Lwt.catch(
        () => handler(reqd, params),
        ex => Lwt.return(Response.error(~status=500, [Printexc.to_string(ex)])),
      )
      >|= (Reqd.request(reqd).meth == `HEAD ? respond_headers(reqd) : respond(reqd))
    )
  };
};

/* The error handler for the httpaf server */
let error_handler = (_, ~request as _=?, error, start_response) => {
  let error_response =
    switch (error) {
    | `Exn(ex) => Response.error(~status=500, [Printexc.to_string(ex)])
    | `Bad_request => Response.error(~status=400, [])
    | `Bad_gateway => Response.error(~status=502, [])
    | `Internal_server_error => Response.error(~status=500, [])
    };
  let response_body = start_response(base_headers(Response.body(error_response)));
  Body.write_string(response_body, Response.body(error_response));
  Body.close_writer(response_body);
};
