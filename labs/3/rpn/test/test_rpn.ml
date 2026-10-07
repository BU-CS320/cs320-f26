open OUnit2
open Rpn

let split_by_ws_tests =
  "split_by_ws tests" >:::
  [
    "empty string" >:: (fun _ ->
      assert_equal [] (split_by_ws ""));
    "all whitespace" >:: (fun _ ->
      assert_equal [] (split_by_ws "\n\n   \n\t\n"));
    "basic example" >:: (fun _ ->
      assert_equal ["foo"; "bar"; "baz"] (split_by_ws "foo bar   \nbaz"))
  ]

let eval_tests =
  (* alternatively you can abstract the individual assertion *)
  let t expected input _ =
    assert_equal
      ~printer:string_of_int
      expected
      (eval input)
  in
  "eval tests" >:::
    [
      "single number" >:: t 123 ["123"];
      "basic add test" >:: t 3 ["1"; "2"; "+"];
    ]

let suite =
  "Rpn calculator test suite" >:::
    [
      split_by_ws_tests;
      eval_tests;
    ]

let _run = run_test_tt_main suite
