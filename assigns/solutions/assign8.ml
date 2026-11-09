open Utils

module Type = struct
  type t =
    | Nat
    | Arr of t * t

  let of_sexpr_opt =
    let rec go : Sexpr.t -> t option = function
      | Atom "Nat" -> Some Nat
      | List [Atom "->"; e1; e2] ->
         begin
           match go e1 with
           | Some e1 ->
              begin
                match go e2 with
                | Some e2 -> Some (Arr (e1, e2))
                | None -> None
              end
           | None -> None
         end
      | _ -> None
    in go
end

module Expr = struct
  type t =
    | Var of string
    | Fun of string * Type.t * t
    | App of t * t
    | Let of string * t * t
    | Zero
    | Succ of t
    | Rec_nat of t * t * t

  let of_sexpr_opt se =
    let var_of_string x =
      if String.for_all (fun c -> is_lower c || c == '_') x
      then Some x
      else None
    in
    let rec expr_of_int n =
      if n = 0
      then Zero
      else Succ (expr_of_int (n - 1))
    in
    let rec go : Sexpr.t -> t option = function
      | Atom x when Option.is_some (int_of_string_opt x) ->
         Some (expr_of_int (int_of_string x))
      | Atom x ->
         begin
           match var_of_string x with
           | Some x -> Some (Var x)
           | None -> None
         end
      | List [Atom "add1"; e] ->
         begin
           match go e with
           | Some e -> Some (Succ e)
           | None -> None
         end
      | List [e1; e2] ->
         begin
           match go e1 with
           | Some e1 ->
              begin
                match go e2 with
                | Some e2 -> Some (App (e1, e2))
                | None -> None
              end
           | None -> None
         end
      | List [Atom "lambda"; Atom x; ty; body] ->
         begin
           match var_of_string x with
           | Some x ->
              begin
                match Type.of_sexpr_opt ty with
                | Some ty ->
                   begin
                     match go body with
                     | Some body -> Some (Fun (x, ty, body))
                     | None -> None
                   end
                | None -> None
              end
           | None -> None
         end
      | List [Atom "let"; Atom x; e1; e2] ->
         begin
           match var_of_string x with
           | Some x ->
              begin
                match go e1 with
                | Some e1 ->
                   begin
                     match go e2 with
                     | Some e2 -> Some (Let (x, e1, e2))
                     | None -> None
                   end
                | None -> None
              end
           | None -> None
         end
      | List [Atom "rec-Nat"; e1; e2; e3] ->
         begin
           match go e1 with
           | Some e1 ->
              begin
                match go e2 with
                | Some e2 ->
                   begin
                     match go e3 with
                     | Some e3 -> Some (Rec_nat (e1, e2, e3))
                     | None -> None
                   end
                | None -> None
              end
           | None -> None
         end
      | _ -> None
    in go se

  let of_string_res s =
    match Sexpr.of_string_opt s with
    | Some se -> Option.to_result ~none:"syntax error" (of_sexpr_opt se)
    | None -> Error "syntax error"

  let of_string s =
    match of_string_res s with
    | Ok e -> e
    | Error _ -> raise (Failure "Expr.of_string")

  let subst e' x =
    let rec go = function
      | Var y ->
         if x = y
         then e'
         else Var y
      | Fun (y, t, e) ->
         if x = y
         then Fun (y, t, e)
         else Fun (y, t, go e)
      | App (e1, e2) -> App (go e1, go e2)
      | Let (y, e1, e2) ->
         if x = y
         then Let (y, go e1, e2)
         else Let (y, go e1, go e2)
      | Zero -> Zero
      | Succ e -> Succ (go e)
      | Rec_nat (e1, e2, e3) -> Rec_nat (go e1, go e2, go e3)
  in go
end

module Ctxt = struct
  module M = Map.Make(String)
  type t = Type.t M.t

  (* empty context

     Ctxt.empty ≡ ∅ ≡ {}
   *)
  let empty = M.empty

  (* Determine declared type for a variable in the context

     Ctxt.find_opt x Γ = { Some τ    if (x : τ) ∈ Γ
                         { None      otherwise
   *)
  let find_opt : string -> t -> Type.t option = M.find_opt

  (* Add a declartion to the context

     Ctxt.add x τ Γ ≡ Γ, x : τ
   *)
  let add : string -> Type.t -> t -> t = M.add
end

let rec type_of (gamma: Ctxt.t) (e : Expr.t) : (Type.t, string) result =
  match e with
  | Var x ->
     begin
       match Ctxt.find_opt x gamma with
       | Some t -> Ok t
       | None -> Error "unknown variable"
     end
  | Fun (x, in_ty, e) ->
     begin
       match type_of (Ctxt.add x in_ty gamma) e with
       | Ok out_ty -> Ok (Arr (in_ty, out_ty))
       | Error e -> Error e
     end
  | App (e1, e2) ->
     begin
       match type_of gamma e1 with
       | Ok Arr (in_ty, out_ty) ->
          begin
            match type_of gamma e2 with
            | Ok ty_2 when ty_2 = in_ty -> Ok out_ty
            | Ok _ -> Error "input type does not match output type"
            | Error e -> Error e
          end
       | Ok _ -> Error "expected function type"
       | Error e -> Error e
     end
  | Let (x, e1, e2) ->
     begin
       match type_of gamma e1 with
       | Ok t1 -> type_of (Ctxt.add x t1 gamma) e2
       | Error e -> Error e
     end
  | Zero -> Ok Nat
  | Succ n ->
     begin
       match type_of gamma n with
       | Ok Nat -> Ok Nat
       | Ok _ -> Error "expected nat (succ)"
       | Error e -> Error e
     end
  | Rec_nat (e1, e2, e3) ->
     begin
       match type_of gamma e1 with
       | Ok Nat ->
          begin
            match type_of gamma e2 with
            | Ok t2 ->
               begin
                 match type_of gamma e3 with
                 | Ok t3 when t3 = Arr (Nat, Arr (t2, t2)) -> Ok t2
                 | Ok _ -> Error "expected step function"
                 | Error e -> Error e
               end
            | Error e -> Error e
          end
       | Ok _ -> Error "expected nat (rec_nat)"
       | Error e -> Error e
     end

let rec eval (e : Expr.t) : Expr.t =
  match e with
  | Var x -> assert false
  | Fun (x, ty, e) -> Fun (x, ty, e)
  | App (e1, e2) ->
     begin
       match eval e1 with
       | Fun (x, _, e) -> eval (Expr.subst (eval e2) x e)
       | _ -> assert false
     end
  | Let (x, e1, e2) -> eval (Expr.subst (eval e1) x e2)
  | Zero -> Zero
  | Succ e -> Succ (eval e)
  | Rec_nat (e, base, step) ->
     begin
       match eval e with
       | Zero -> eval base
       | Succ n -> eval (App (App (step, n), Rec_nat (n, base, step)))
       | _ -> assert false
     end

let string_of_value e =
  let rec int_of_value_opt : Expr.t -> int option = function
    | Zero -> Some 0
    | Succ n ->
       begin
         match int_of_value_opt n with
         | Some n -> Some (n + 1)
         | None -> None
       end
    | _ -> None
  in
  match int_of_value_opt e with
  | Some n -> string_of_int n
  | None -> "<fun>"

let interp s =
  match Expr.of_string_res s with
  | Ok e ->
     begin
       match type_of Ctxt.empty e with
       | Ok _ -> Format.printf "Success: %s" (string_of_value (eval e))
       | Error e -> Format.eprintf "Error: %s" e
     end
  | Error e -> Format.eprintf "Error: %s" e
