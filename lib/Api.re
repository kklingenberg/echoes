open Lwt.Infix;
module EchoesVersion = Version;
open Httpaf;

module Log = Dolog.Log;

let base_headers = body =>
  Headers.of_list([
    /* CORS headers */
    ("Access-Control-Allow-Credentials", "true"),
    ("Access-Control-Allow-Origin", "*"),
    /* HTTP 1.1 connection header */
    ("Connection", "keep-alive"),
    /* Everything here is JSON */
    ("Content-Type", "application/json"),
    ("Content-Length", string_of_int(String.length(body))),
    /* Date of the response */
    (
      "Date",
      CalendarLib.Printer.Calendar.sprint(
        "%a, %d %b %Y %H:%M:%S GMT",
        CalendarLib.Calendar.to_gmt(CalendarLib.Calendar.now()),
      ),
    ),
    /* Server version */
    ("Server", Printf.sprintf("echoes/%s", EchoesVersion.version)),
  ]);

module Response = {
  type t = (string, int);
  let body = fst;
  let status = snd;
  let raw = (~status: int=200, b: string): t => (b, status);
  let json = (~status: int=200, b: Yojson.Safe.t) => (
    Yojson.Safe.to_string(b),
    status,
  );
  let error = (~status: int=500, details: list(string)) => (
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
    status,
  );
};

let respond = (reqd: Reqd.t, response: Response.t) =>
  Reqd.respond_with_string(
    reqd,
    Httpaf.Response.create(
      ~headers=base_headers(Response.body(response)),
      Status.of_code(Response.status(response)),
    ),
    Response.body(response),
  );

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

  let get = (p, h) => (regexp_of_routepath(p), `GET, h);
  let head = (p, h) => (regexp_of_routepath(p), `HEAD, h);
  let post = (p, h) => (regexp_of_routepath(p), `POST, h);
  let put = (p, h) => (regexp_of_routepath(p), `PUT, h);
  let delete = (p, h) => (regexp_of_routepath(p), `DELETE, h);
};

let path_syntax =
  Hashtbl.find(Neturl.common_url_syntax, "http") |> Neturl.partial_url_syntax;

let make_request_handler = (routes, _, reqd) => {
  let url = Neturl.url_of_string(path_syntax, Reqd.request(reqd).target);
  Log.debug(
    "%s %s",
    Method.to_string(Reqd.request(reqd).meth),
    Neturl.string_of_url(url),
  );
  let path = Neturl.url_path(url) |> Neturl.norm_path |> Neturl.join_path;
  let matched =
    List.filter_map(
      route => {
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
      },
      routes,
    );
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
  let route = List.nth_opt(matched_routes, 0);
  switch (route) {
  | None when List.length(matched) == 0 =>
    respond(
      reqd,
      Response.error(
        ~status=404,
        [Printf.sprintf("No route matched the given path: %s", path)],
      ),
    )
  | None =>
    respond(
      reqd,
      Response.error(
        ~status=405,
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
      >|= respond(reqd)
    )
  };
};

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
