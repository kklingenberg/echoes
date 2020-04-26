open Api;

let routes: list(Route.t) = [
  Route.get("/foo/bar/baz", (_, _) => Lwt.return(Response.make(Response.of_string("")))),
  Route.get("/foo/bar/baz/zomg/", (_, _) => Lwt.return(Response.make(Response.of_string("")))),
  Route.get("/test/:w/words/and/:p/paths", (_, _) => Lwt.return(Response.make(Response.of_string("")))),
];
