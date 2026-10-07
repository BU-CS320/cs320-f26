open Linalg

let ( let* ) = Result.bind

let _main =
  let output =
    let* a = Mat.mk 3 3
      [ [1.; 2.; 3.]
      ; [4.; 5.; 6.]
      ; [7.; 8.; 9.]
      ]
    in
    let* v = Vec.mk 3 [1.; 2.; 3.] in
    mat_vec_mul a v
  in
  match output with
  | Ok v -> Format.printf "%s" (Vec.to_string string_of_float v)
  | Error err -> Format.printf "Error: %s" (Error.to_string err)
