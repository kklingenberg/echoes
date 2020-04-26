/*open Lwt.Infix;*/
open Httpaf;

module Log = Dolog.Log;

module Response = {
  type body =
    | Simple(string)
    | Stream(int => Lwt.t(option(string)));
  let of_string = b => Simple(b);
  let of_stream = b => Stream(b);
  type t = (body, int, Headers.t);
  let make = (~status: int=200, ~headers: Headers.t=Headers.empty, b: body): t => (
    b,
    status,
    headers,
  );
};

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
  let route =
    List.nth_opt(
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
      ),
      0,
    );
  switch (route) {
  | None when List.length(matched) == 0 =>
    Reqd.respond_with_string(
      reqd,
      Httpaf.Response.create(
        ~headers=Headers.of_list([("Connection", "close")]),
        `Not_found,
      ),
      "Not found!",
    )
  | None =>
    Reqd.respond_with_string(
      reqd,
      Httpaf.Response.create(
        ~headers=Headers.of_list([("Connection", "close")]),
        `Method_not_allowed,
      ),
      "Wrong method!",
    )
  | Some(_) =>
    Reqd.respond_with_string(
      reqd,
      Httpaf.Response.create(
        ~headers=Headers.of_list([("Connection", "close")]),
        `OK,
      ),
      "All good!",
    )
  };
};

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
