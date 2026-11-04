
(* Syntax *)

(*
  τ ::= ⊥ | τ → τ
  e ::= λ x . e | e e | x
 *)

type ty =
  | B
  | Arr of ty * ty

type expr =
  | Lam of string * ty * expr
  | App of expr * expr
  | Var of string

(* Typing *)

type ctxt = (string * ty) list

let rec type_of (gamma : ctxt) (e : expr) : ty option =
  match e with
    (*
       (x : τ) ∈ Γ
       ────────────(var)
        Γ ⊢ x : τ
     *)
  | Var x -> List.assoc_opt x gamma
  (*
         Γ, x : τ₁ ⊢ e : τ₂
    ─────────────────────────────(fun)
    Γ ⊢ λ (x : τ₁) . e : τ₁ → τ₂
   *)
  | Lam (x, t1, e) ->
     begin
       match type_of ((x, t1) :: gamma) e with
       | Some t2 -> Some (Arr (t1, t2))
       | None -> None
     end
  (*
    Γ ⊢ e₁ : τ₂ → τ    Γ ⊢ e₂ : τ₂
    ──────────────────────────────(app)
             Γ ⊢ e₁ e₂ : τ
   *)
  | App (e1, e2) ->
     begin
       match type_of gamma e1 with
       | Some (Arr (t2, t)) ->
          begin
            match type_of gamma e2 with
            | Some t2' when t2 = t2' -> Some t
            | _ -> None
          end
       | _ -> None
     end

(* Semantics *)


let subst (v : expr) (x : string) (e : expr) : expr =
  let rec go e =
    match e with
    (* [v / x] y = { v    x = y
                   { y    otherwise
     *)
    | Var y ->
       if x = y
       then v
       else Var y
    (*
      [v / x](λ (y : τ) . e) = { λ (y : τ) . e           x = y
                               { λ (y : τ) . [v / x] e   otherwise
     *)
    | Lam (y, t, e) ->
       if x = y
       then Lam (y, t, e)
       else Lam (y, t, go e)
    (*
      [v / x](e₁ e₂) = ([v / x] e₁) ([v / x] e₂)
     *)
    | App (e1, e2) -> App (go e1, go e2)
  in go e

let rec eval (e : expr) : expr =
  match e with
  (* no rule *)
  | Var x -> raise (Failure "unknown variable")
  (*
    ──────────────────(funE)
    λ x . e ⇓ λ x . e
   *)
  | Lam (x, ty, e) -> Lam (x, ty, e)
  (*
    e₁ ⇓ λ x . e     e₂ ⇓ v₂     [v₂ / x]e ⇓ v
    ────────────────────────────────────────────(appE)
                     e₁ e₂ ⇓ v
   *)
  | App (e1, e2) ->
     match eval e1 with
     | Lam (x, _, e) ->
        let v2 = eval e2 in
        let v = eval (subst v2 x e) in
        v
     | _ -> raise (Failure "expected function")

let interp e =
  match type_of [] e with
  | Some _ -> eval e
  | None -> raise (Failure "whoops")
