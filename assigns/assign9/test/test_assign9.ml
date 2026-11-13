open OUnit2
open Assign9

let expr s = Option.get (Expr.of_string_opt s)

let pat s =
  s
  |> Sexpr.of_string_opt
  |> Option.get
  |> Pattern.of_sexpr_opt
  |> Option.get

let ty s =
  s
  |> Sexpr.of_string_opt
  |> Option.get
  |> Type.of_sexpr_opt
  |> Option.get

let type_pat_tests =
  let t bindings p ty_str _ =
    assert_equal
      (Ok (List.map (fun (x, t) -> (x, ty t)) bindings))
      (Result.map VarMap.bindings (type_pat (pat p) (ty ty_str)))
  in
  let t_err err p ty_str _ =
    assert_equal
      (Error err)
      (Result.map VarMap.bindings (type_pat (pat p) (ty ty_str)))
  in
  "type_pat tests" >:::
    [
      "cons" >::
        t ["x", "int"; "xs", "(list int)"] "(:: x xs)" "(list int)";
      "nested cons" >::
        t ["x", "bool"; "y", "bool"] "(:: x (:: y _))" "(list bool)";
      "int against bool" >:: t_err Ty_error.(Wrong_type (Int, Bool)) "1" "bool";
      "nil against int" >:: t_err Ty_error.(Unexpected_list_pat Int) "[]" "int";
      "bound several times" >::
        t_err Ty_error.Bound_several_times "(:: x x)" "(list (list int))";
      (* add more tests *)
    ]

let type_of_tests =
  let t ty_str e _ =
    assert_equal
      (Ok (ty ty_str))
      (type_of Ctxt.empty (expr e))
  in
  let t_err err e _ =
    assert_equal
      (Error err)
      (type_of Ctxt.empty (expr e))
  in
  "type_of tests" >:::
    [
      "recursive let" >::
        t "int"
          "(let sum (-> (list int) int)
             (lambda l (list int)
               (match l
                 (([] 0)
                  ((:: x xs) (+ x (sum xs))))))
             (sum (:: 1 (:: 2 (:: 3 ([] int))))))";
      "non-recursive let" >:: t_err Ty_error.(Unknown_var "x") "(let x int x 1)";
      "apply non-function" >:: t_err Ty_error.(Expected_fun Int) "(1 2)";
      "arms disagree" >::
        t_err Ty_error.(Wrong_type (Bool, Int)) "(match 1 ((0 true) (_ 1)))";
      "bad pattern" >:: t_err Ty_error.(Unexpected_list_pat Int) "(match 1 (([] 0)))";
      (* add more tests *)
    ]

let matches_tests =
  (* environments are compared by their bindings so that the internal
     shape of the map doesn't matter *)
  let t bindings v p _ =
    assert_equal
      (Some bindings)
      (Option.map VarMap.bindings (matches v (pat p)))
  in
  let t_none v p _ =
    assert_equal
      None
      (Option.map VarMap.bindings (matches v (pat p)))
  in
  "matches tests" >:::
    [
      "cons" >::
        t ["x", Value.Int 1; "xs", Value.(Cons (Int 2, Nil))]
          (Value.Cons(Int 1, Cons (Int 2, Nil)))
          "(:: x xs)";
      "nested cons" >::
        t ["x", Value.Int 1; "y", Value.Int 2]
          (Value.Cons (Int 1, Cons (Int 2, Cons (Int 3, Nil))))
          "(:: x (:: y _))";
      "int mismatch" >:: t_none (Value.Int 3) "4";
      "nil against cons" >:: t_none Value.Nil "(:: x xs)";
      "too short" >:: t_none (Value.Cons (Int 1, Nil)) "(:: x (:: y _))";
      (* add more tests *)
    ]

let eval_tests =
  let t expected e _ =
    assert_equal
      expected
      (eval Env.empty (expr e))
  in
  "eval tests" >:::
    [
      "closure" >::
        t (Value.Int 3)
          "(let x int 1
             (let f (-> int int) (lambda y int (+ x y))
               (let x int 100 (f 2))))";
      "match first arm" >::
        t (Value.Int 0) "(match 0 ((0 0) (_ 1)))";
      "sum" >::
        t (Value.Int 6)
          "(let sum (-> (list int) int)
             (lambda l (list int)
               (match l
                 (([] 0)
                  ((:: x xs) (+ x (sum xs))))))
             (sum (:: 1 (:: 2 (:: 3 ([] int))))))";
      "fact" >::
        t (Value.Int 120)
          "(let fact (-> int int)
             (lambda n int
               (if (<= n 0) 1 (* n (fact (- n 1)))))
             (fact 5))";
      "map" >::
        t (Value.Cons (Int 2, Cons (Int 3, Cons (Int 4, Nil))))
          "(let map (-> (-> int int) (-> (list int) (list int)))
             (lambda f (-> int int)
               (lambda l (list int)
                 (match l
                   (([] ([] int))
                    ((:: x xs) (:: (f x) ((map f) xs)))))))
             ((map (lambda x int (+ x 1))) (:: 1 (:: 2 (:: 3 ([] int))))))";
      (* add more tests *)
    ]

let suite =
  "assign9 tests" >:::
    [
      type_pat_tests;
      type_of_tests;
      matches_tests;
      eval_tests;
    ]

let _run_tests = run_test_tt_main suite
