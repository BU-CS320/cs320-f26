open Assign3

let testing = true

let run cases b = if b then cases () else []

let nub_tests () =
  [
    assert (nub [(1, "1"); (2, "2")] = [(1, "1"); (2, "2")]);
    assert (nub [(1, "1"); (2, "2"); (1, "3")] = [(1, "1"); (2, "2")]);
    assert (nub [(1, "1"); (2, "2"); (1, "3"); (1, "4")] = [(1, "1"); (2, "2")]);
    assert (nub [(2, "2"); (1, "3"); (1, "4"); (2, "5")] = [(2, "2"); (1, "3")]);
    (* add more tests here *)
  ]

let split_tests () =
  [
    assert (split_by_ws' "1 23 + -94  *" = ["1"; "23"; "+"; "-94"; "*"]);
    assert (split_by_ws' "  foo    bar  \n\n   baz   " = ["foo"; "bar"; "baz"]);
    (* add more tests here *)
  ]

let _run_tests =
  if not testing then [] else
    [
      run nub_tests true;
      run split_tests true;
    ]
