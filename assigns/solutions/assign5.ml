
let sign n = if n >= 0 then 1 else -1

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

type 'a rtree = Node of 'a * 'a rtree list

let split_list xs =
  let rec go acc_l acc_r xs =
    match xs with
    | [] -> List.rev acc_l, List.rev acc_r
    | (x, y) :: xs -> go (x :: acc_l) (y :: acc_r) xs
  in go [] [] xs

let rec split (Node ((a, b), ts) : ('a * 'b) rtree) : 'a rtree * 'b rtree =
  let ts_l, ts_r =
    ts
    |> List.map split
    |> split_list
  in
  Node (a, ts_l), Node (b, ts_r)

let prefix_map (f : 'a list -> 'b option) (l : 'a list) : ('b * 'a list) option =
  let rec go pre rest =
    match f (List.rev pre) with
    | Some b -> Some (b, rest)
    | None ->
       match rest with
       | [] -> None
       | x :: xs -> go (x :: pre) xs
  in go [] l

let apply_cycle (fs : ('a -> 'a) list) (n : int) (x : 'a) : 'a =
  let rec go f fs n x =
    if n = 0
    then x
    else
      match fs with
      | [] -> go f [] (n - 1) (f x)
      | g :: gs -> go g (gs @ [f]) (n - 1) (f x)
  in
  match fs with
  | [] -> assert false
  | f :: fs -> go f fs n x

let walks (f : 'a -> 'a -> bool) (n : int) (ps : (('a -> 'a) * 'a) list) : 'a list =
  let rec go n ps =
    if n = 0
    then List.map (fun (_, x) -> x) ps
    else
      ps
      |> List.map (fun (g, x) -> g, g x, f x (g x))
      |> List.filter (fun (_, _, valid) -> valid)
      |> List.map (fun (g, x, _) -> g, x)
      |> go (n - 1)
  in go n ps
