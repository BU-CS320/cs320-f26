
let not b = if b then false else true
let abs n = if n < 0 then -n else n

let rec list_equal eq l r =
  match l, r with
  | [], [] -> true
  | x :: xs, y :: ys -> eq x y && list_equal eq xs ys
  | _ -> false

(* Problem 1: Number of digits *)

let rec num_digits_positive n =
  if n < 10
  then 1
  else 1 + num_digits_positive (n / 10)

let num_digits n = num_digits_positive (abs n)

(* Problem 2: Perfect powers *)

let rec pow n k =
  if k = 0
  then 1
  else n * pow n (k - 1)

let rec is_perfect_pow i n =
  if i = 0
  then n = 1
  else if i < 0
  then
    if abs i mod 2 = 1
    then abs n = 1
    else n = 1
  else if n < 0
  then is_perfect_pow i (abs n) && i mod 2 = 1
  else
    let rec go m =
      if pow m i = n
      then true
      else if pow m i > n
      then false
      else go (m + 1)
    in go 0

(* Problem 3: Number of factors *)

let num_factors k =
  let rec go acc i n =
    if i > n
    then acc
    else if n mod i = 0
    then go (acc + 1) i (n / i)
    else go acc (i + 1) n
  in
  go 0 2 k

(* Problem 4: hypotenuse *)

let is_hypotenuse c =
  let rec go a b =
    if a >= c
    then false
    else
      let diff = c * c - a * a - b * b in
      if diff = 0
      then true
      else if diff < 0
      then go (a + 1) (c - a - 1)
      else go a (b + 1)
  in go 1 (c - 1)

(* Problem 5: Drop leading *)

let rec drop_leading k l =
  match l with
  | [] -> []
  | x :: xs ->
    if x = k
    then drop_leading k xs
    else l

(* Tests *)

let _num_digits_tests =
  [
    assert (num_digits 0 = 1);
    assert (num_digits 7 = 1);
    assert (num_digits 12345 = 5);
    assert (num_digits (-121) = 3);
  ]

let _is_perfect_pow_tests =
  [
    assert (is_perfect_pow 1 16);
    assert (is_perfect_pow 2 16);
    assert (not (is_perfect_pow 3 16));
    assert (is_perfect_pow 4 16);
    assert (is_perfect_pow 3 (-8));
  ]

let _is_hypotenuse_tests =
  [
    assert (is_hypotenuse 5);
    assert (is_hypotenuse 13);
    assert (is_hypotenuse 17);
    assert (is_hypotenuse 29);
    assert (not (is_hypotenuse 28));
    assert (not (is_hypotenuse 6));
  ]

let _drop_leading_tests =
  let eq = list_equal (fun x y -> x = y) in
  [
    assert (eq (drop_leading 1 [1;1;2;1;3]) [2;1;3]);
    assert (eq (drop_leading 5 [1;2;3]) [1;2;3]);
    assert (eq (drop_leading 2 [2;2;2]) []);
  ]
