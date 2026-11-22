
let rev l =
  let rec go acc l =
    match l with
    | [] -> acc
    | x :: l -> go (x :: acc) l
  in go [] l

let rec list_equal eq l r =
  match l, r with
  | [], [] -> true
  | x :: xs, y :: ys -> eq x y && list_equal eq xs ys
  | _ -> false

(* Problem 1: Nub *)

let rec remove_all key l =
  let rec go acc l =
    match l with
    | [] -> rev acc
    | (k, v) :: l ->
      if k = key
      then go acc l
      else go ((k, v) :: acc) l
  in go [] l

let rec nub l =
  match l with
  | [] -> []
  | (k, v) :: l -> (k, v) :: nub (remove_all k l)

let _nub_tests =
  let eq =
    list_equal
      (fun (x1, y1) (x2, y2) -> x1 = x2 && y1 = y2)
  in
  [
    assert (eq (nub [(1, 1); (2, 2)]) [(1, 1); (2, 2)]);
    assert (eq (nub [(1, 1); (2, 2); (1, 3)]) [(1, 1); (2, 2)]);
    assert (eq (nub [(1, 1); (2, 2); (1, 3); (1, 4)]) [(1, 1); (2, 2)]);
    assert (eq (nub [(2, 2); (1, 3); (1, 4); (2, 5)]) [(2, 2); (1, 3)]);
  ]
