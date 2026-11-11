
type expr = Syntax.expr =
  | Var of string
  | Num of int
  | Fun of string * expr
  | Add of expr * expr
  | Eq of expr * expr
  | If of expr * expr * expr
  | App of expr * expr
  | Let of string * expr * expr

let parse (s : string) : expr option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | e -> Some e
  | exception _ -> None

module Env = Map.Make (String)

type value =
  | VNum of int
  | VBool of bool
  | VClos of env * string * expr * string option

and env = value Env.t

(* ⟨ E, e ⟩ ⇓ v *)
let rec eval (env : env) (e : expr) : value =
  let num v =
    match v with
    | VNum n -> n
    | _ -> assert false
  in
  let bool v =
    match v with
    | VBool b -> b
    | _ -> assert false
  in
  let vclos v =
    match v with
    | VClos (env, x, e, name) -> env, x, e, name
    | _ -> assert false
  in
  match e with
  | Var x -> Env.find x env
  | Num n -> VNum n
  | Fun (x, e) -> VClos (env, x, e, None)
  | Add (e1, e2) ->
     begin
       let v1 = num (eval env e1) in
       let v2 = num (eval env e2) in
       VNum (v1 + v2)
     end
  | Eq (e1, e2) ->
     begin
       let v1 = num (eval env e1) in
       let v2 = num (eval env e2) in
       VBool (v1 = v2)
     end
  | If (e1, e2, e3) ->
     if (bool (eval env e1))
     then eval env e2
     else eval env e3
  | App (e1, e2) ->
     let env', x, e, name = vclos (eval env e1) in
     let v2 = eval env e2 in
     begin
       match name with
       | None ->
          eval (Env.add x v2 env') e
       | Some name ->
          eval
            (env
             |> Env.add name (VClos (env', x, e, Some name))
             |> Env.add x v2)
            e
     end
  | Let (x, Fun (x1, e1), e2) ->
     eval
       (Env.add x (VClos (env, x1, e1, Some x)) env)
       e2
  | Let (x, e1, e2) ->
     let v1 = eval env e1 in
     eval (Env.add x v1 env) e2

let eval = eval Env.empty

let interp (s : string) : (value, string) result =
  match parse s with
  | Some expr -> Ok (eval expr) (* TODO: Type check *)
  | None -> Error "whoops"
