
let is_ws c = c = ' ' || c = '\n' || c = '\r' || c = '\t' || c = '\012'

let split_by_ws s =
  let rec go acc i =
    if i = String.length s
    then List.rev acc
    else if is_ws s.[i]
    then go acc (i + 1)
    else go' acc i 1
  and go' acc i len =
    if i + len = String.length s || is_ws s.[i + len]
    then go (String.sub s i len :: acc) (i + len)
    else go' acc i (len + 1)
  in go [] 0

type dir = N | S | E | W

let step (x, y) dir =
  match dir with
  | N -> (x + 1, y)
  | S -> (x - 1, y)
  | E -> (x, y + 1)
  | W -> (x, y - 1)

let rec steps pos dirs =
  match dirs with
  | [] -> pos
  | dir :: dirs -> steps (step pos dir) dirs

let dist dirs =
  let (x, y) = steps (0, 0) dirs in
  let x = float_of_int x in
  let y = float_of_int y in
  sqrt (x *. x +. y *. y)

type int_or_string
  = Int of int
  | String of string

type int_list_or_string_list
  = Int_list of int list
  | String_list of string list

let cons x xs =
  match x, xs with
  | Int n, Int_list ns :: l -> Int_list (n :: ns) :: l
  | Int n, l -> Int_list [n] :: l
  | String n, String_list ns :: l -> String_list (n :: ns) :: l
  | String n, l -> String_list [n] :: xs

let rec convert l =
  match l with
  | [] -> []
  | x :: xs -> cons x (convert xs)

type 'a tree
  = Empty
  | Node of 'a * 'a tree * 'a tree

let rec insert x t =
  match t with
  | Empty -> Node (x, Empty, Empty)
  | Node (y, l, r) ->
    if x <= y
    then Node (y, insert x l, r)
    else Node (y, l, insert x r)

let rec flatten t =
  match t with
  | Empty -> []
  | Node (x, l, r) ->
    flatten l @ [x] @ flatten r

let rec sort l =
  let rec mk_tree t l =
    match l with
    | [] -> t
    | x :: xs -> mk_tree (insert x t) xs
  in flatten (mk_tree Empty l)
