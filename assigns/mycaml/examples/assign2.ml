let not b = if b then false else true

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

let bool_equal b1 b2 = (b1 && b2) || (not b1 && not b2)

(* Problem 1: Taxicab numbers *)

let cube n = n * n * n

let is_cube n =
  let rec go i =
    let i3 = i * i * i in
    if i3 = n
    then true
    else if i3 > n
    then false
    else go (i + 1)
  in go 0

let taxicab (n : int) : int =
  let rec go acc a =
    if cube a > n - cube a
    then acc
    else if is_cube (n - cube a)
    then go (acc + 1) (a + 1)
    else go acc (a + 1)
  in go 0 1

(* Problem 2: Drop trailing *)

let rec drop_leading k l =
  match l with
  | [] -> []
  | x :: xs ->
    if x = k
    then drop_leading k xs
    else l

let drop_trailing k l =
  rev (drop_leading k (rev l))

(* Problem 3: Every k elements *)

let every_k k l =
  let rec go acc i l =
    match l with
    | [] -> acc
    | x :: xs ->
      if i = 1
      then go (x :: acc) k xs
      else go acc (i - 1) xs
  in rev (go [] 1 l)

(* Problem 4: Bitonic sequences *)

let rec is_monotonic inc l =
  match l with
  | [] -> true
  | x :: xs -> (
    match xs with
    | [] -> true
    | y :: ys ->
      let correct_dir = if inc then x < y else x > y in
      correct_dir && is_monotonic inc xs
  )

let rec is_bitonic inc l =
  match l with
  | [] -> true
  | x :: xs -> (
    match xs with
    | [] -> true
    | y :: ys ->
      if x = y
      then false
      else
        let correct_dir = if inc then x < y else x > y in
        if correct_dir
        then is_bitonic inc xs
        else is_monotonic (not inc) xs
  )

let is_bitonic l =
  match l with
  | [] -> true
  | [_] -> true
  | x :: y :: xs ->
    if x < y
    then is_bitonic true l
    else is_bitonic false l

(* Problem 5: Factoring *)

let factor n =
  let rec go acc k n =
    if k > n
    then acc
    else
      let rec go_pow count n =
        if n mod k = 0
        then go_pow (count + 1) (n / k)
        else (n, count)
      in
      let (n, count) = go_pow 0 n in
      let acc =
        if count > 0
        then (k, count) :: acc
        else acc
      in
      go acc (k + 1) n
  in rev (go [] 2 n)

(* Problem 6: Valid 1D paths *)

let step p =
  match p with
  | l, [] -> l, []
  | l, (dir, len) :: xs ->
    l + (if dir then 1 else -1) * len, xs

let rec is_valid p1 p2 =
  match p1, p2 with
  | (l1, []), (l2, []) -> l1 <> l2
  | p1, p2 ->
    let (l1, steps1) = step p1 in
    let (l2, steps2) = step p2 in
    l1 < l2 && is_valid (l1, steps1) (l2, steps2)

let is_valid p1 p2 =
  let (l1, _) = p1 in
  let (l2, _) = p2 in
  if l1 > l2
  then is_valid p2 p1
  else if l1 < l2
  then is_valid p1 p2
  else false

let _taxicab_tests =
  [
    assert (taxicab 2 = 1);
    assert (taxicab 5 = 0);
    assert (taxicab 1729 = 2);
    assert (taxicab 4104 = 2);
  ]

let _drop_trailing_tests  =
  let eq = list_equal (fun x y -> x = y) in
  [
    assert (eq (drop_trailing 3 [1;2;3;3]) [1;2]);
    assert (eq (drop_trailing 5 [1;2;3]) [1;2;3]);
    assert (eq (drop_trailing 2 [2;2;2]) []);
  ]

let _every_k_tests =
  let eq = list_equal (fun x y -> x = y) in
  [
    assert (eq (every_k 1 [1;2;3;4;5]) [1;2;3;4;5]);
    assert (eq (every_k 2 [1;2;3;4;5]) [1;3;5]);
    assert (eq (every_k 3 [1;2;3;4;5]) [1;4]);
    assert (eq (every_k 4 [1;2;3;4;5]) [1;5]);
    assert (eq (every_k 5 [1;2;3;4;5]) [1]);
  ]

let _is_bitonic_tests =
  let eq = bool_equal in
  [
    assert (eq (is_bitonic [1;2;3;2;1]) true);
    assert (eq (is_bitonic [1;2;3]) true);
    assert (eq (is_bitonic [3;2;1;2]) true);
    assert (eq (is_bitonic []) true);
    assert (eq (is_bitonic [1;1]) false);
    assert (eq (is_bitonic [1;2;1;2]) false);
  ]

let _factor_tests =
  let eq = list_equal (fun (x1, y1) (x2, y2) -> x1 = x2 && y1 = y2) in
  [
    assert (eq (factor 1) []);
    assert (eq (factor 19) [(19,1)]);
    assert (eq (factor 16) [(2,4)]);
    assert (eq (factor 360) [(2,3);(3,2);(5,1)]);
  ]

let _is_valid_tests =
  let p1 = (0, [(true, 3); (false, 5)]) in
  let p2 = (5, [(false, 3); (false, 7)]) in

  let p3 = (0, [(true, 6); (false, 12)]) in
  let p4 = (5, [(true, 2); (false, 12)]) in

  let p5 = (0, [(true, 6)]) in

  [
    assert (not (is_valid (10, []) (10, [])));
    assert (not (is_valid p2 p4));
    assert (is_valid (0, []) (10, []));
    assert (not (is_valid p1 p2));
    assert (is_valid p3 p4);
    assert (not (is_valid p5 p4));
  ]
