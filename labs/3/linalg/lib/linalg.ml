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
    let actual_num_rows = List.length elems in
    if actual_num_rows <> num_rows
    then Error (Mismatch_num_rows (num_rows, actual_num_rows))
    else
      match
        elems
        |> List.mapi (fun i row -> (i, List.length row))
        |> List.find_opt (fun (_, len) -> len <> num_cols)
      with
      | Some (index, actual_size) ->
        let expected_size = num_cols in
        Error (Mismatch_row_size {index; expected_size; actual_size})
      | None -> Ok (mk_unsafe num_rows num_cols elems)

  let num_rows a = a.num_rows
  let num_cols a = a.num_cols
  let elems a = a.elems

  let rows a = List.map (Vec.mk_unsafe (num_rows a)) (elems a)
end

let dot_unsafe (v1 : float Vec.t) (v2 : float Vec.t) : float =
  List.fold_left (+.) 0. (List.map2 ( *. ) (Vec.elems v1) (Vec.elems v2))

let dot (v1 : float Vec.t) (v2 : float Vec.t) : (float, Error.t) result =
  if Vec.size v1 <> Vec.size v2
  then Error (Invalid_dot (Vec.size v1, Vec.size v2))
  else Ok (dot_unsafe v1 v2)

let mat_vec_mul_unsafe (a : float Mat.t) (v : float Vec.t) : float Vec.t =
  let _ = a, v in
  assert false (* TODO *)

let mat_vec_mul (a : float Mat.t) (v : float Vec.t) : (float Vec.t, Error.t) result  =
  let _ = a, v in
  assert false (* TODO *)
