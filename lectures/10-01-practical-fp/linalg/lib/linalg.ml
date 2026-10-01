module Error = Error

module Vec = struct
  type 'a t = {
    size : int;
    elems : 'a list;
  }

  (* unsafe operations don't check if the input is well-formed *)
  let mk_unsafe size elems = {size; elems}

  let mk (size : int) (elems : 'a list) : ('a t, Error.t) result  =
    let actual_size = List.length elems in
    if actual_size <> size
    then Error (Mismatch_size (size, actual_size))
    else Ok (mk_unsafe size elems)

  let size v = v.size
  let elems v  = v.elems

  let to_string string_of_a v =
    v
    |> elems
    |> List.map string_of_a
    |> String.concat "\n"
end

module Mat = struct
  type 'a t = {
    num_rows : int;
    num_cols : int;
    elems : 'a list list;
  }

  let mk_unsafe num_rows num_cols elems = {num_rows; num_cols; elems}

  let mk
      (num_rows : int)
      (num_cols : int)
      (elems : 'a list list)
    : ('a t, Error.t) result =
    let _ = num_rows, num_cols, elems in
    assert false (* TODO *)
  (* look at List.mapi, List.find_opt *)

  let num_rows a = a.num_rows
  let num_cols a = a.num_cols
  let elems a = a.elems

  let rows a = List.map (Vec.mk_unsafe (num_rows a)) (elems a)
end

let dot_unsafe (v1 : float Vec.t) (v2 : float Vec.t) : float =
  let _ = v1, v2 in
  assert false (* TODO *)
(* look at List.fold_left, List.map2 *)

let dot (v1 : float Vec.t) (v2 : float Vec.t) : (float, Error.t) result =
  let _ = v1, v2 in
  assert false (* TODO *)

let mat_vec_mul_unsafe (a : float Mat.t) (v : float Vec.t) : float Vec.t =
  let _ = a, v in
  assert false (* TODO *)

let mat_vec_mul (a : float Mat.t) (v : float Vec.t) : (float Vec.t, Error.t) result  =
  let _ = a, v in
  assert false (* TODO *)
