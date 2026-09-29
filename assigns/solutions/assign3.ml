
let rec remove_all key l =
  let rec go acc l =
    match l with
    | [] -> List.rev acc
    | (k, v) :: l ->
      if k = key
      then go acc l
      else go ((k, v) :: acc) l
  in go [] l

let rec nub l =
  match l with
  | [] -> []
  | (k, v) :: l -> (k, v) :: nub (remove_all k l)

let explode (s : string) : char list =
  let rec loop acc i =
    if i = String.length s
    then acc
    else loop (s.[i] :: acc) (i + 1)
  in List.rev (loop [] 0)

let implode (l : char list) : string =
  String.init (List.length l) (List.nth l)

let is_ws (c : char) : bool =
  c = ' '
  || c = '\n'
  || c = '\r'
  || c = '\t'
  || c = '\012'

let split_by_ws' (s : string) : string list =
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
