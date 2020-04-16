type t =
  | Encoded(string, string);

let from_raw = s =>
  switch (s) {
  | "" => Error(["empty raw string for actor type"])
  | _ =>
    Ok(
      Encoded(
        s,
        Netencoding.Base64.encode(s)
        |> Str.global_replace(Str.regexp("\\+"), "-")
        |> Str.global_replace(Str.regexp("/"), "_"),
      ),
    )
  };

let from_encoded = s =>
  switch (
    s
    |> Str.global_replace(Str.regexp("_"), "/")
    |> Str.global_replace(Str.regexp("-"), "+")
    |> Netencoding.Base64.decode
  ) {
  | raw when String.length(raw) > 0 => Ok(Encoded(raw, s))
  | exception _ => Error(["invalid encoded string for actor type"])
  | _ => Error(["empty encoded string for actor type"])
  };

let to_raw = (Encoded(raw, _)) => raw;
let to_encoded = (Encoded(_, encoded)) => encoded;

let key_lookup: t => string =
  actor =>
    Printf.sprintf("%s%s:*", Settings.current^.redis_key_prefix, to_encoded(actor));
