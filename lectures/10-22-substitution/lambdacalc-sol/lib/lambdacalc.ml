open Syntax

module Value = struct
  type t =
    | Vfun of string * Expr.t

  let to_expr (Vfun (x, e)) = Expr.Fun (x, e)
end

let parse s = Syntax.Expr.of_string s

let ( let* ) = Option.bind

let subst v x e =
  let rec go : Expr.t -> Expr.t = function
    | Fun (y, e') -> if x = y then Fun (y, e') else Fun (y, go e')
    | App (e1, e2) -> App (go e1, go e2)
    | Var y -> if x = y then Value.to_expr v else Var y
  in go e

let rec eval (e : Expr.t) : Value.t option =
  match e with
  | Fun (x, e) -> Some (Vfun (x, e))
  | App (e1, e2) ->
     begin
       match eval e1 with
       | Some Vfun (x, e) ->
          begin
            match eval e2 with
            | Some v2 -> eval (subst v2 x e)
            | _ -> None
          end
       | _ -> None
     end
  | _ -> None

let interp s =
  match parse s with
  | None -> print_endline "parse error"
  | Some e ->
     match eval e with
     | None -> print_endline "eval error"
     | Some v ->
        v
        |> Value.to_expr
        |> Expr.to_string
        |> print_endline
