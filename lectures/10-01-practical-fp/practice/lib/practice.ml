
(* let rec foo check get_val l = *)
(*   match l with *)
(*   | [] -> [] *)
(*   | x :: xs -> (if check x then [get_val x] else []) *)
(*                @ foo check get_val xs *)

let rec negatives = List.filter_map (fun x -> if x < 0 then Some x else None)
let rec gets k = List.filter_map (fun (key, v) -> if key = k then Some v else None)

let rec fold_right (op : 'a -> 'b -> 'b) (l : 'a list) (base : 'b) : 'b =
  match l with
  | [] -> base
  | x :: xs -> op x (fold_right op xs base)

let filter (f : 'a -> bool) (l : 'a list) : 'a list =
  List.fold_right
    (fun curr acc -> if f curr then curr :: acc else acc)
    l
    []

let append (l : 'a list) (r : 'a list) : 'a list = List.fold_right (fun x xs -> x :: xs) l r
