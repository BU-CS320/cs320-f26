
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
  let _ = s in
  assert false (* TODO *)

let eval (p : string list) : int =
  let _ = p in
  assert false (* TODO *)

let interp (s : string) : int =
  let prog = split_by_ws s in
  eval prog
