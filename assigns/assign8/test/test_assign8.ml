open OUnit2
open Assign8

let type_of_tests =
  let t ty e _ =
    assert_equal
      (Ok ty)
      (type_of Ctxt.empty (Expr.of_string e))
  in
  let t_err e _ =
    assert_bool
      "expected type error"
      (Result.is_error (type_of Ctxt.empty (Expr.of_string e)))
  in
  "type_of tests" >:::
    [
      "id" >::
        t Type.(Arr (Nat, Nat)) "(lambda x Nat x)";
      "app" >::
        t Type.(Arr (Nat, Nat))
          "((lambda f (-> Nat Nat) (lambda x Nat (f (f x)))) (lambda y Nat (add1 y)))";
      "rec-Nat" >::
        t Type.Nat
          "(let step (lambda p Nat (lambda x Nat (add1 x))) (rec-Nat 3 4 step))";
      "unknown variable" >:: t_err "(lambda x Nat y)";
      "bad step" >:: t_err "(rec-Nat 3 0 (lambda x Nat x))";
      (* add more tests *)
    ]

let eval_tests =
  let t expected e _ =
    assert_equal
      (Expr.of_string expected)
      (eval (Expr.of_string e))
  in
  "eval tests" >:::
    [
      "add1" >:: t "14" "(add1 (let x (add1 11) (add1 x)))";
      "let" >:: t "6" "(let x 5 (let y (add1 x) y))";
      "shadowing" >:: t "2" "(let x 1 ((lambda x Nat (add1 x)) x))";
      "rec-Nat zero" >:: t "7" "(rec-Nat 0 7 (lambda p Nat (lambda x Nat 0)))";
      "add" >::
        t "42"
          "(let add
             (lambda m Nat
               (lambda n Nat
                 (let step (lambda p Nat (lambda x Nat (add1 x)))
                 (rec-Nat m n step))))
           ((add 15) 27))";
      (* add more tests *)
    ]

let suite =
  "stlc interpreter tests" >:::
    [
      type_of_tests;
      eval_tests;
    ]

let _run_tests = run_test_tt_main suite
