open Utils

module Sexpr = Sexpr

module Expr = struct
  type t =
    | Fun of string * t
    | App of t * t
    | Var of string

  let to_string e =
    let rec go = function
      | Fun (x, e) -> Format.sprintf "(lambda %s %s)" x (go e)
      | App (e1, e2) -> Format.sprintf "(%s %s)" (go e1) (go e2)
      | Var x -> x
    in go e

  let ( let* ) = Option.bind

  let of_sexpr_opt se = ignore se; assert false

  let of_string_opt s =
    let* se = Sexpr.of_string_opt s in
    of_sexpr_opt se

  let of_string s =
    match of_string_opt s with
    | Some e -> e
    | None -> raise (Failure "Expr.of_string")

  let rec fv : t -> VarSet.t = function
    | Fun (x, e) -> VarSet.remove x (fv e)
    | App (e1, e2) -> VarSet.union (fv e1) (fv e2)
    | Var x -> VarSet.singleton x

  let subst e' x e = ignore (e', x, e); assert false
  let equal e1 e2 = ignore (e1, e2); assert false
end

let step e = ignore e; assert false

let multistep =
  let rec go e =
    match step e with
    | Some e -> go e
    | None -> e
  in go

let interp s =
  match Expr.of_string_opt s with
  | None -> print_endline "parse error"
  | Some e ->
     match multistep e with
     | e -> e |> Expr.to_string |> print_endline

let expr_of_int n = ignore n; assert false

let int_of_expr_opt e = ignore e; assert false

let int_of_expr e =
  match int_of_expr_opt e with
  | Some n -> n
  | None -> raise (Failure "int_of_expr")

let suc : Expr.t = Expr.Var "x" (* TODO *)
let add : Expr.t = Expr.Var "x" (* TODO *)
