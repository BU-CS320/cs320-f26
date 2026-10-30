let is_lower c = c >= 'a' && c <= 'z'

let is_ws = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false
