open Api;

let routes: list(Route.t) = [
  Route.get("/foo/bar/baz", (_, _) => Lwt.return(Response.raw("null"))),
  Route.get("/foo/bar/baz/zomg/", (_, _) => Lwt.return(Response.raw("true"))),
  Route.get("/test/:w/words/and/:p/paths", (_, _) =>
    Lwt.return(Response.raw("false"))
  ),
];
