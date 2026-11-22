(* unit *)
let _ = ()
let _ : unit = ()

(* bool *)
let _ = true
let _ : bool = false

(* var *)
let x = true
let y = x

(* negate *)
let _ = -1
let _ : int = -(-2)

(* bop *)
let _ = 1 + 2
let _ = 3 - 4
let _ = 1 - 2 - 3
let _ = 1 - 2 + 3
let _ = 3 * 4
let _ = 3 * 4 + 1
let _ = 2 + 3 * 4
let _ = 8 / 3
let _ = 12 / 2 / 2
let _ = 3 * 4 / 3
let _ = 3 * (4 / 3)
let _ = true && true
let _ = true && false
let _ = false && true
let _ = false && false
let _ = false && assert false
let _ = true || true
let _ = true || false
let _ = false || true
let _ = false || false
let _ = true || assert false

(* if *)
let _ = if true then 1 else assert false
let _ = if false then assert false else 2

(* tuple *)
let _ = (1, 2)
let _ = (1, true, ())
let _ = 1, true, 3, false, ()

(* assert *)
let _ = assert true
let _ = assert (if false then false else true)

(* fun *)
let f = fun x -> x
let g = fun (x, y) -> x + y

(* app *)
let _ = f 1
let _ = f true
let _ = g (3, 4)
let g = fun x y -> x + y
let h = g 1
let i = h 2

(* let *)
let _ = let f x = 2 in f 10
let _ =
  let f x = 1 in
  let g y = 2 in
  f () + g ()
let _ =
  let f x = 1 in
  let f y = 2 in
  f () + f ()
let _ =
  let g x y = x + y in
  let h = g 1 in
  h 0

(* let_pat *)
let _ = let z = 2 in z

(* match *)
let _ =
  match 1 + 2 with
  | 2 -> 100
  | _ -> 4
let _ =
  match () with
  | () -> 0
let _ =
  let b = true in
  match b with
  | false -> 0
  | true -> 1
let _ =
  match 5 + 6 with
  | 1 -> 0
  | 11 -> 100
  | _ -> 1
let _ =
  match 11 + 12 with
  | x -> x + 3
let _ =
  match (1, 2, 3) with
  | (x, _, y) -> x + y
let _ =
  match true && false with
  | (b : bool) -> b
let _ = [1;2;3]
let _ =
  match [1;2;3] with
  | x :: y :: _ -> x + y
  | _ -> 0
let _ =
  match [1;2;3] with
  | [x;_;z] -> x + z
  | _ -> assert false
let _ =
  match [1;2] :: [3;4] :: [] with
  | [x;_] :: (_  :: y :: _) :: _ -> x + y
  | _ -> assert false

let foo l =
  match l with
  | [] -> []
  | _ :: xs -> (
    match xs with
    | [] -> []
    | _ :: xs -> xs
  )

let _ = foo [1;2;3;4;5]

let rec fact n =
  match n with
  | 0 -> 1
  | n -> n * fact (n - 1)

let _ = fact 1

(* recursive lets *)
let rec length l =
  match l with
  | [] -> 0
  | _ :: xs -> 1 + length xs

let _ = length [1;2;3;4;5]
