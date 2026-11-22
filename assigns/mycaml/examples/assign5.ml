
type 'a option =
  | None
  | Some of 'a

let sign n = if n >= 0 then 1 else -1

let rev l =
  let rec go acc l =
    match l with
    | [] -> acc
    | x :: l -> go (x :: acc) l
  in go [] l

let map f l =
  let rec go acc l =
    match l with
    | [] -> rev acc
    | x :: xs -> go (f x :: acc) xs
  in go [] l

let filter f l =
  let rec go acc l =
    match l with
    | [] -> rev acc
    | x :: xs ->
       if f x
       then go (x :: acc) xs
       else go acc xs
  in go [] l

let rec concat l r =
  match l with
  | [] -> r
  | x :: l -> x :: (concat l r)

(* Problem 1: Grouping *)

let rec group (l : int list) : int list list option =
  match l with
  | [] -> Some []
  | x :: 0 :: y :: xs ->
     if sign x <> sign y
     then
       match group (y :: xs) with
       | None -> None
       | Some xs -> Some ([x] :: xs)
     else None
  | 0 :: xs -> None
  | [x] -> Some [[x]]
  | x :: y :: xs ->
     if sign x = sign y
     then
       match group (y :: xs) with
       | None -> None
       | Some [] -> None
       | Some (l :: xs) -> Some ((x :: l) :: xs)
     else None

(* Problem 2: Splitting rose trees *)

type 'a rtree =
  | Node of 'a * 'a rtree list

let split_list xs =
  let rec go acc_l acc_r xs =
    match xs with
    | [] -> rev acc_l, rev acc_r
    | (x, y) :: xs -> go (x :: acc_l) (y :: acc_r) xs
  in go [] [] xs

let rec split (Node ((a, b), ts) : ('a * 'b) rtree) : 'a rtree * 'b rtree =
  let ts_l, ts_r = split_list (map split ts) in
  Node (a, ts_l), Node (b, ts_r)

let prefix_map (f : 'a list -> 'b option) (l : 'a list) : ('b * 'a list) option =
  let rec go pre rest =
    match f (rev pre) with
    | Some b -> Some (b, rest)
    | None -> (
       match rest with
       | [] -> None
       | x :: xs -> go (x :: pre) xs
    )
  in go [] l

let apply_cycle (fs : ('a -> 'a) list) (n : int) (x : 'a) : 'a =
  let rec go f fs n x =
    if n = 0
    then x
    else
      match fs with
      | [] -> go f [] (n - 1) (f x)
      | g :: gs -> go g (concat gs [f]) (n - 1) (f x)
  in
  match fs with
  | [] -> assert false
  | f :: fs -> go f fs n x

let walks (f : 'a -> 'a -> bool) (n : int) (ps : (('a -> 'a) * 'a) list) : 'a list =
  let rec go n ps =
    if n = 0
    then map (fun (_, x) -> x) ps
    else
      let ps = map (fun (g, x) -> g, g x, f x (g x)) ps in
      let ps = filter (fun (_, _, valid) -> valid) ps in
      let ps = map (fun (g, x, _) -> g, x) ps in
      go (n - 1) ps
  in go n ps
