open Mycaml
open OUnit2


let suite =
  "mycaml test suite" >:::
    [
    ]

let _ = run_test_tt_main suite
