
let cube n = n * n * n

let is_cube n =
  let k = int_of_float (float_of_int n ** (1. /. 3.)) in
  cube k = n || cube (k + 1) = n

let taxicab (n : int) : int =
  let rec go acc a =
    if cube a > n - cube a
    then acc
    else if is_cube (n - cube a)
    then go (acc + 1) (a + 1)
    else go acc (a + 1)
  in go 0 1

let rec drop_leading k l =
  match l with
  | [] -> []
  | x :: xs ->
    if x = k
    then drop_leading k xs
    else l

let drop_trailing k l =
  List.rev (drop_leading k (List.rev l))

let every_k k l =
  let rec go acc i l =
    match l with
    | [] -> acc
    | x :: xs ->
      if i = 1
      then go (x :: acc) k xs
      else go acc (i - 1) xs
  in List.rev (go [] 1 l)

let rec is_monotonic inc l =
  match l with
  | [] -> true
  | x :: xs ->
    match xs with
    | [] -> true
    | y :: ys ->
      let correct_dir = if inc then x < y else x > y in
      correct_dir && is_monotonic inc xs

let rec is_bitonic inc l =
  match l with
  | [] -> true
  | x :: xs ->
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

let is_bitonic l =
  match l with
  | [] | [_] -> true
  | x :: y :: xs ->
    if x < y
    then is_bitonic true l
    else is_bitonic false l

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
  in List.rev (go [] 2 n)

type path = int * (bool * int) list

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
