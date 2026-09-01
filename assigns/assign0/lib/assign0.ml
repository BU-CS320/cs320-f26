let sqrt (n : int) : int =
  let rec loop (k : int) : int =
    if n <= k * k
    then k
    else loop (k + 1)
  in loop 0

let is_prime (n : int) : bool =
  let rec loop (i : int) : bool =
    if i >= n
    then true
    else if n mod i = 0
    then false
    else loop (i + 1)
  in
  if n < 2
  then false
  else loop 2
