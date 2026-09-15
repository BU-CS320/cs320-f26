
let digits_of_int (n : int) : int list =
  let rec go n =
    if 0 <= n && n <= 9
    then [n]
    else n mod 10 :: go (n / 10)
  in List.rev (go (abs n))

let int_of_digits (ds : int list) : int =
  let rec go ds =
    match ds with
    | [] -> 0
    | d :: ds -> d + 10 * go ds
  in go (List.rev ds)

let digits_of_int_tr (n : int) : int list =
  let rec go acc n =
    if 0 <= n && n <= 9
    then n :: acc
    else go (n mod 10 :: acc) (n / 10)
  in go [] (abs n)

let int_of_digits_tr (ds : int list) : int =
  let rec go acc ds =
    match ds with
    | [] -> acc
    | d :: ds -> go (d + 10 * acc) ds
  in go 0 ds

let rec gcd (i : int) (j : int) : int =
  if i = j
  then i
  else if i > j
  then gcd (i - j) j
  else gcd i (j - i)

let py_triples (n : int) : (int * int * int) list =
  let rec go acc i j k =
    let acc =
      if i * i + j * j = k * k && gcd i (gcd j k) = 1
      then (i, j, k) :: acc
      else acc
    in
    if k < n
    then go acc i j (k + 1)
    else if j < n
    then go acc i (j + 1) (j + 1)
    else if i < n
    then go acc (i + 1) (i + 1) (i + 1)
    else List.rev acc
  in go [] 1 1 1
