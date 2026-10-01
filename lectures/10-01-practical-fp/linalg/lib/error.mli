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

val to_string : t -> string
