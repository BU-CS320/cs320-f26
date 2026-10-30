open Utils

module Type = struct
  type t =
    | Nat
    | Arr of t * t

  let of_sexpr_opt (e : Sexpr.t) : t option = ignore e; assert false (* could be useful... *)
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

  let of_sexpr_opt se = ignore se; assert false (* TODO *)

  let of_string_res s =
    match Sexpr.of_string_opt s with
    | Some se -> Option.to_result ~none:"syntax error" (of_sexpr_opt se)
    | None -> Error "syntax error"

  let of_string s =
    match of_string_res s with
    | Ok e -> e
    | Error _ -> raise (Failure "Expr.of_string")

  let subst (e' : t) (x : string) (e : t) : t = ignore (e', x, e); assert false (* TODO *)
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
  ignore (gamma, e); assert false (* TODO *)

let rec eval (e : Expr.t) : Expr.t =
  ignore e; assert false (* TODO *)

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

let interp (s : string) : unit =
  match Expr.of_string_res s with
  | Ok e ->
     begin
       match type_of Ctxt.empty e with
       | Ok _ -> Format.printf "Success: %s" (string_of_value (eval e))
       | Error e -> Format.eprintf "Error: %s" e
     end
  | Error e -> Format.eprintf "Error: %s" e
