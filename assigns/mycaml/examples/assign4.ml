
let rec concat l r =
  match l with
  | [] -> r
  | x :: l -> x :: (concat l r)

let rec list_equal eq l r =
  match l, r with
  | [], [] -> true
  | x :: xs, y :: ys -> eq x y && list_equal eq xs ys
  | _ -> false

(* Problem 3: Grouping intergers and strings *)

type int_or_bool =
  | Int of int
  | Bool of bool

type int_list_or_bool_list =
  | Int_list of int list
  | Bool_list of bool list

let cons x xs =
  match x, xs with
  | Int n, Int_list ns :: l -> Int_list (n :: ns) :: l
  | Int n, l -> Int_list [n] :: l
  | Bool n, Bool_list ns :: l -> Bool_list (n :: ns) :: l
  | Bool n, l -> Bool_list [n] :: xs

let rec convert l =
  match l with
  | [] -> []
  | x :: xs -> cons x (convert xs)

let _example =
  convert
    [
      Int 2;
      Int 3;
      Bool true;
      Bool false;
      Int 4;
      Bool true
    ]

(* Problem 4: Binary tree insertion, flattening, sorting *)

type 'a tree =
  | Empty
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
    concat (concat (flatten l) [x]) (flatten r)

let rec sort l =
  let rec mk_tree t l =
    match l with
    | [] -> t
    | x :: xs -> mk_tree (insert x t) xs
  in flatten (mk_tree Empty l)

let _sort_test =
  let eq = list_equal (fun x y -> x = y) in
  assert (eq (sort [5;3;8;1;4]) [1;3;4;5;8])
