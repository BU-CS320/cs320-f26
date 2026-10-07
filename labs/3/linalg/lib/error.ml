type t =
  | Mismatch_size of int * int
  | Mismatch_num_rows of int * int
  | Mismatch_row_size of {
      index : int;
      expected_size : int;
      actual_size : int;
    }
  | Invalid_dot of int * int
  | Invalid_mat_vec_mul of {
      num_cols : int;
      vec_size : int;
    }

let to_string (e : t) : string =
  match e with
  | Mismatch_size (expected, actual) ->
    String.concat " "
      [ "attempted to make a vector of size"
      ; string_of_int expected
      ; "out of"
      ; string_of_int actual
      ; "elements"
      ]
  | Mismatch_num_rows (expected, actual) ->
    String.concat " "
      [ "attempted to make a matrix with"
      ; string_of_int expected
      ; "rows using"
      ; string_of_int actual
      ; "rows"
      ]
  | Mismatch_row_size data ->
    String.concat " "
      [ "attempted to make matrix with"
      ; string_of_int data.expected_size
      ; "columns but row"
      ; string_of_int data.index
      ; "has"
      ; string_of_int data.actual_size
      ; "elements"
      ]
  | Invalid_dot (left_size, right_size) ->
    String.concat " "
      [ "attempted to take dot product of a vector with"
      ; string_of_int left_size
      ; "elements and one with"
      ; string_of_int right_size
      ; "elements"
      ]
  | Invalid_mat_vec_mul data ->
    String.concat " "
      [ "attempted to multiply a matrix with"
      ; string_of_int data.num_cols
      ; "columns and a vector with"
      ; string_of_int data.vec_size
      ; "elements"
      ]
