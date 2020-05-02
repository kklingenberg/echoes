type t =
  | Safe(string);

let min_length = 12;
let max_length = 64;

module CharSet = Set.Make(Char);
let base64_alphabet: CharSet.t = {
  let letters = "abcdefghijklmnopqrstuvwxyz";
  letters
  ++ String.uppercase_ascii(letters)
  ++ "0123456789-_="
  |> String.to_seq
  |> List.of_seq
  |> CharSet.of_list;
};
let is_safe_char = c => CharSet.mem(c, base64_alphabet);

let from_string = raw =>
  if (String.length(raw) < min_length) {
    Error(["idempotency key is too short"]);
  } else if (String.length(raw) > max_length) {
    Error(["idempotency key is too long"]);
  } else if (!(raw |> String.to_seq |> List.of_seq |> List.for_all(is_safe_char))) {
    Error(["idempotency key has invalid characters"]);
  } else {
    Ok(Safe(raw));
  };

let to_string = (Safe(s)) => s;
