open OUnit2
open Assign7

let of_sexpr_opt_tests =
  "of_sexpr_opt tests" >:::
    [
      "variable" >:: (fun _ ->
        assert_equal
          (Expr.of_sexpr_opt (Sexpr.Atom "x"))
          (Some (Expr.Var "x")));
      "function" >:: (fun _ ->
        assert_equal
          (Expr.of_sexpr_opt (Sexpr.List [Sexpr.Atom "lambda"; Sexpr.Atom "x"; Sexpr.Atom "x"]))
          (Some (Expr.Fun ("x", Var "x"))));
      "application" >:: (fun _ ->
        assert_equal
          (Expr.of_sexpr_opt (Sexpr.List [Sexpr.List [Sexpr.Atom "x"; Sexpr.Atom "y"]; Sexpr.Atom "z"]))
          (Some (Expr.App (Expr.App (Var "x", Var "y"), Var "z"))));
      (* add more tests *)
    ]

let equal_tests =
  let t e1 e2 _ =
    assert_bool
      "not alpha equivalent"
      (Expr.(equal (of_string e1) (of_string e2)))
  in
  "equal tests" >:::
    [
      "basic" >:: t "(lambda x (x y))" "(lambda z (z y))";
      (* add more tests *)
    ]

let subst_tests =
  let t expected e2 x e1 _ =
    assert_equal
      ~cmp:Expr.equal
      expected
      (Expr.subst e2 x e1)
  in
  "subst tests" >:::
    [
      "variable" >::
        t (Expr.Var "y")
          (Expr.Var "y")
          "x"
          (Expr.Var "x");
      "basic capture" >::
        t Expr.(Fun ("x", Fun ("z", App (Var "x", Var "y"))))
          Expr.(Var "y")
          "z"
          Expr.(Fun ("x", Fun ("y", App (Var "x", Var "z"))));
      (* add more tests *)
    ]

let step_tests =
  let cmp e1 e2 =
    match e1, e2 with
    | Some e1, Some e2 -> Expr.equal e1 e2
    | None, None -> true
    | _ -> false
  in
  let t expected input _ =
    assert_equal
      ~cmp
      expected
      (step input)
  in
  "step test" >:::
    [
      "normal form" >::
        t None
          Expr.(Fun ("x", Var "x"));
      "under binder" >::
        t (Some Expr.(Fun ("y", Var "y")))
          (Expr.Fun ("y", App (Fun ("x", Var "x"), Var "y")));
      (* add more tests *)
    ]

let expr_of_int_tests =
  let t expected input _ =
    assert_equal
      ~cmp:Expr.equal
      (Expr.of_string expected)
      (expr_of_int input)
  in
  "expr_of_int tests" >:::
    [
      "zero" >:: t "(lambda f (lambda x x))" 0;
      "one" >:: t "(lambda f (lambda x (f x)))" 1;
      "two" >:: t "(lambda f (lambda x (f (f x))))" 2;
    ]

let suite =
  "lambda calculus test suite" >:::
    [
      of_sexpr_opt_tests;
      equal_tests;
      subst_tests;
      step_tests;
      expr_of_int_tests;
      (* add more test groups *)
    ]

let _run_tests = run_test_tt_main suite
