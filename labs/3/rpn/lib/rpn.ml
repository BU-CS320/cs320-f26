
let explode (s : string) : char list =
  let rec loop acc i =
    if i = String.length s
    then acc
    else loop (s.[i] :: acc) (i + 1)
  in List.rev (loop [] 0)

let implode (l : char list) : string =
  String.init (List.length l) (List.nth l)

let is_ws (c : char) : bool = c = ' ' || c = '\n' || c = '\r' || c = '\t' || c = '\012'

let split_by_ws (s : string) : string list =
  let rec go acc cs =
    match cs with
    | [] -> List.rev acc
    | c :: cs ->
      if is_ws c
      then go acc cs
      else go_in_word acc [c] cs
  and go_in_word acc rev_word cs =
    match cs with
    | [] -> List.rev (implode (List.rev rev_word) :: acc)
    | c :: cs ->
      if is_ws c
      then go (implode (List.rev rev_word) :: acc) cs
      else go_in_word acc (c :: rev_word) cs
  in go [] (explode s)

let rec eval (stack : int list) (prog : string list) =
  match stack, prog with
  | [out], _ -> out
  | y :: x :: stack, "+" :: prog -> eval (x + y :: stack) prog
  | y :: x :: stack, "-" :: prog -> eval (x - y :: stack) prog
  | y :: x :: stack, "*" :: prog -> eval (x * y :: stack) prog
  | y :: x :: stack, "/" :: prog -> eval (x * y :: stack) prog
  | stack, num :: prog -> eval (int_of_string num :: stack) prog
  | _ -> failwith "whoops"

let eval p = eval [] p

let interp (s : string) : int =
  let prog = split_by_ws s in
  eval prog
