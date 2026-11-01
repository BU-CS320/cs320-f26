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

  let of_sexpr_opt se =
    let var_of_string s =
      if String.for_all is_lower s
      then Some s
      else None
    in
    let rec go : Sexpr.t -> t option = function
      | Atom x ->
         let* x = var_of_string x in
         Some (Var x)
      | List [Atom "lambda"; Atom x; body] ->
         let* x = var_of_string x in
         let* body = go body in
         Some (Fun (x, body))
      | List [e1; e2] ->
         let* e1 = go e1 in
         let* e2 = go e2 in
         Some (App (e1, e2))
      | _ -> None
    in go se

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

  let rec subst e' x e =
    let rec go : t -> t = function
      | Fun (y, e1) ->
         if x = y
         then Fun (y, e1)
         else
           if VarSet.mem y (fv e')
           then
             let z = gensym () in
             Fun (z, go (subst (Var z) y e1))
           else Fun (y, go e1)
      | App (e1, e2) -> App (go e1, go e2)
      | Var y -> if x = y then e' else Var y
    in go e

  let equal =
    let rec go e1 e2 =
      match e1, e2 with
      | Var x, Var y -> x = y
      | App (e11, e12), App (e21, e22) -> go e11 e21 && go e12 e22
      | Fun (x, e1), Fun (y, e2) ->
         let z = gensym () in
         go (subst (Var z) x e1) (subst (Var z) y e2)
      | _ -> false
    in go

end


let step e =
  let rec go : Expr.t -> Expr.t option = function
    | Fun (x, e) -> let* e = go e in Some (Expr.Fun (x, e))
    | App (Fun (x, e1), e2) -> Some (Expr.subst e2 x e1)
    | App (e1, e2) ->
       begin
         match go e1 with
         | Some e1 -> Some (App (e1, e2))
         | None -> let* e2 = go e2 in Some (Expr.App (e1, e2))
       end
    | Var x -> None
  in go e

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

let expr_of_int n =
  let rec go = function
    | 0 -> Expr.Var "x"
    | n -> Expr.App (Var "f", go (n - 1))
  in Expr.Fun ("f", Fun ("x", go n))

let int_of_expr_opt (e : Expr.t) =
  let rec go : Expr.t -> (int * string * string) option = function
    | Var x -> Some (0, "f", x)
    | App (Var f, Var x) -> if f = x then None else Some (1, f, x)
    | App (Var f, e) ->
       let* n, g, x = go e in
       if f = g
       then Some (n + 1, f, x)
       else None
    | _ -> None
  in
  match e with
  | Fun (f, Fun (x, e)) ->
     let* n, g, y = go e in
     if (n = 0 || f = g) && x = y
     then Some n
     else None
  | _ -> None

let int_of_expr e =
  match int_of_expr_opt e with
  | Some n -> n
  | None -> raise (Failure "int_of_expr")

let suc : Expr.t = Expr.of_string "(lambda m (lambda f (lambda x (f ((m f) x)))))"
let add : Expr.t = Expr.of_string "(lambda m (lambda n (lambda f (lambda x ((m f) ((n f) x))))))"
