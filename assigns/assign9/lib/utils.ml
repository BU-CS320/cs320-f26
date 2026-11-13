
let is_int w = Option.is_some (int_of_string_opt w)
let is_lower c = c >= 'a' && c <= 'z'

let is_ws = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let is_var w =
  String.for_all (fun c -> is_lower c || c = '_') w
  && String.length w > 0
  && w.[0] <> '_'
