
let rec num_digits_positive n =
  if n < 10
  then 1
  else 1 + num_digits_positive (n / 10)

let rec num_digits n = num_digits_positive (abs n)

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

let num_factors k =
  let rec go acc i n =
    if i > n
    then acc
    else if n mod i = 0
    then go (acc + 1) i (n / i)
    else go acc (i + 1) n
  in
  go 0 2 k

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

let rec drop_leading k l =
  match l with
  | [] -> []
  | x :: xs ->
    if x = k
    then drop_leading k xs
    else l
