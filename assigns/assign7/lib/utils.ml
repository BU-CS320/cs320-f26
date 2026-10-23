let is_lower c = c >= 'a' && c <= 'z'

let is_ws = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let ( let* ) = Option.bind

let gensym =
  let i = ref 0 in
  fun () -> i := !i + 1; "$" ^ string_of_int !i

module VarSet = Set.Make (String)
