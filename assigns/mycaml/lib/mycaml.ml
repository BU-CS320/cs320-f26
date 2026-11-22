open Utils
open Ast

(* ERRORS =================================================================== *)

module Error_msg = Error_msg

module Error = struct
  let dummy msg = Error_msg.mk Loc.dummy msg
  let dummy_loc loc msg = Error_msg.mk loc msg

  let expected_ty loc expected actual =
    let expected, actual =
      match Type.(normalize (tuple expected.meta [expected; actual])).ty with
      | Tuple [expected; actual] -> expected, actual
      | _ -> assert false (* IMPOSSIBLE *)
    in
    let msg =
      Format.sprintf
        "This has type %s but was expected to be of type %s"
        (Type.to_string actual)
        (Type.to_string expected)
    in
    Error_msg.mk loc msg

  let bound_several_times loc x =
    let msg =
      Format.sprintf
        "Variable %s is bound several times in this matching"
        x
    in
    Error_msg.mk loc msg

  let unbound_value loc name =
    let msg =
      Format.sprintf
        "Unbound value %s"
        name
    in
    Error_msg.mk loc msg

  let unbound_constr loc name =
    let msg =
      Format.sprintf
        "Unbound constructor %s"
        name
    in
    Error_msg.mk loc msg

  let constr_expected_arg loc name =
    let msg =
      Format.sprintf
        "The constructor %s expected 1 argument, but is applied here to 0 arguments"
        name
    in
    Error_msg.mk loc msg

  let constr_unexpected_arg loc name =
    let msg =
      Format.sprintf
        "The constructor %s expected 0 argument, but is applied here to 1 arguments"
        name
    in
    Error_msg.mk loc msg


  let too_many_args loc f_ty =
    let msg =
      Format.sprintf
        "This function has type %s. It is applied to too many arguments"
        (Type.to_string f_ty)
    in
    Error_msg.mk loc msg

  let not_a_function loc ty =
    let msg =
      Format.sprintf
        "This expression has type %s. This is not a function; it cannot be applied."
        (Type.to_string ty)
    in
    Error_msg.mk loc msg

  let type_param_occurs_several_times loc =
    let msg = "A type parameter occurs several times" in
    Error_msg.mk loc msg

  let dup_type_name loc name =
    let msg = Format.sprintf "Type %s is already defined" name in
    Error_msg.mk loc msg

  let unbound_ty_var loc name =
    let msg =
      Format.sprintf
        "The type variable %s is unbound in this type declaration"
        name
    in
    Error_msg.mk loc msg

  let unbound_ty_constr loc name =
    let msg = Format.sprintf "Unbound type constructor %s" name in
    Error_msg.mk loc msg

  let ty_constr_arity loc name expected actual =
    let msg =
      Format.sprintf
        "The type constructor %s expects %d argument(s), but is here applied to %d argument(s)"
        name
        expected
        actual
    in
    Error_msg.mk loc msg
end

(* CONTEXTS ================================================================= *)

module Type_scheme = struct
  type t = Forall of unit Ident.t list * unit Type.t

  let mk (xs : unit Ident.t list) (t : unit Type.t) : t =
    Forall (xs, Type.map_meta (fun _ -> ()) t)

  let of_type (t : unit Type.t) : t = mk [] t

  let instantiate (Forall (xs, t) : t) : unit Type.t =
    List.fold_left
      (fun acc (a : unit Ident.t) ->
        Type.subst (Type.fresh_param ()) a.name acc)
      t
      xs

  let to_string (Forall (xs, t) : t) : string = Type.to_string t
end

module Ctxt = struct
  type t = Type_scheme.t SMap.t

  let empty = SMap.empty
  let add = SMap.add
  let find_opt = SMap.find_opt

  let merge (c1 : t) (c2 : t) : t =
    let merger _ t1 t2 =
      match t1, t2 with
      | _, Some ty -> Some ty
      | t1, _ -> t1
    in
    SMap.merge merger c1 c2
end

(* UNIFIERS ================================================================= *)

module Unifier = struct
  type t = (string * unit Type.t) list

  let empty : t = []

  let apply (sol : t) (t : unit Type.t) : unit Type.t =
    List.fold_right
      (fun (x, t) acc -> Type.subst t x acc)
      sol
      t

  let principal (sol : t) (ty : unit Type.t) : Type_scheme.t =
    let ty =
      ty
      |> apply sol
      |> Type.normalize
    in
    Type_scheme.mk (Type.fv ty) ty

  let rec unify
            (loc : Loc.t)
            (t1 : unit Type.t)
            (t2 : unit Type.t)
            (sol : t)
          : (t, Error_msg.t) result =
    let t1 = apply sol t1 in
    let t2 = apply sol t2 in
    match t1.ty, t2.ty with
    | Param a, Param a' when Ident.equal a a' -> Ok sol
    | Param a, _ ->
       if not (List.mem a (Type.fv t2))
       then Ok ((a.name, t2) :: sol)
       else Error (Error.expected_ty loc t1 t2)
    | _, Param a ->
       if not (List.mem a (Type.fv t1))
       then Ok ((a.name, t1) :: sol)
       else Error (Error.expected_ty loc t1 t2)
    | Unit, Unit -> Ok sol
    | Bool, Bool -> Ok sol
    | Int, Int -> Ok sol
    | Fun (in_ty1, out_ty1), Fun (in_ty2, out_ty2) ->
       let* sol = unify loc in_ty1 in_ty2 sol in
       unify loc out_ty1 out_ty2 sol
    | Tuple ts1, Tuple ts2 ->
       if List.(length ts1 = length ts2)
       then unify_list loc (List.combine ts1 ts2) sol
       else Error (Error.expected_ty loc t1 t2)
    | List t1, List t2 -> unify loc t1 t2 sol
    | Adt (ts1, name1), Adt (ts2, name2) when name1 = name2 ->
       if List.(length ts1 = length ts2)
       then unify_list loc (List.combine ts1 ts2) sol
       else Error (Error.expected_ty loc t1 t2)
    | _ -> Error (Error.expected_ty loc t1 t2)
  and unify_list
        (loc : Loc.t)
        (eqs : (unit Type.t * unit Type.t) list)
        (sol : t) : (t, Error_msg.t) result =
    match eqs with
    | [] -> Ok sol
    | (t1, t2) :: eqs ->
       let* sol = unify loc t1 t2 sol in
       unify_list loc eqs sol
end

(* TYPING =================================================================== *)

(* You can ignore this unless you want to get the type errors right *)
let bound_adt_names (ctxt : Ctxt.t) : (string * int) list =
  SMap.fold
    (fun _ (Forall (_, t) : Type_scheme.t) acc ->
      match t.ty with
      | Adt (ts, n) | Fun (_, { ty = Adt (ts, n); _ }) ->
         (n.name, List.length ts) :: acc
      | _ -> acc)
    ctxt
    []

(* You can ignore this unless you want to get the type errors right *)
let rec check_ty
          (names : (string * int) list)
          (ty : Loc.t Type.t)
        : (unit, Error_msg.t) result =
  match ty.ty with
  | Unit | Bool | Int | Param _ -> Ok ()
  | Fun (t1, t2) -> check_tys names [t1; t2]
  | List t -> check_ty names t
  | Tuple ts -> check_tys names ts
  | Adt (ts, n) -> (
    match List.assoc_opt n.name names with
    | None -> Error (Error.unbound_ty_constr n.meta n.name)
    | Some arity when arity <> List.length ts ->
       Error (Error.ty_constr_arity ty.meta n.name arity (List.length ts))
    | Some _ -> check_tys names ts
  )
and check_tys
      (names : (string * int) list)
      (ts : Loc.t Type.t list)
    : (unit, Error_msg.t) result =
  match ts with
  | [] -> Ok ()
  | t :: ts ->
     let* () = check_ty names t in
     check_tys names ts

let rec type_of_pattern
          (ctxt : Ctxt.t)
          (p : Loc.t Pattern.t)
          (new_bindings : Ctxt.t)
          (sol : Unifier.t)
        : (unit Type.t * Ctxt.t * Unifier.t, Error_msg.t) result =
  match p.pattern with
  | Wild -> Ok (Type.fresh_param (), new_bindings, sol)
  | Unit -> Ok (Type.unit (), new_bindings, sol)
  | Bool _ -> Ok (Type.bool (), new_bindings, sol)
  | Int _ -> Ok (Type.int (), new_bindings, sol)
  | Var x -> (
    match Ctxt.find_opt x.name new_bindings with
    | None ->
       let a = Type.fresh_param () in
       Ok (a, Ctxt.add x.name (Type_scheme.of_type a) new_bindings, sol)
    | Some _ -> Error (Error.bound_several_times x.meta x.name)
  )
  | Tuple ps ->
     let* ps_tys, new_bindings, sol = type_of_pattern_list ctxt ps new_bindings sol in
     Ok (Type.tuple () ps_tys, new_bindings, sol)
  | Annot (p, ty) ->
     let* () = check_ty (bound_adt_names ctxt) ty in
     let* p_ty, new_bindings, sol = type_of_pattern ctxt p new_bindings sol in
     let* sol = Unifier.unify p.meta (Type.erase ty) p_ty sol in
     Ok (p_ty, new_bindings, sol)
  | Cons (p, ps) ->
     let* p_ty, new_bindings, sol = type_of_pattern ctxt p new_bindings sol in
     let* ps_ty, new_bindings, sol = type_of_pattern ctxt ps new_bindings sol in
     let* sol = Unifier.unify ps.meta (Type.list () p_ty) ps_ty sol in
     Ok (ps_ty, new_bindings, sol)
  | List_lit [] -> Ok (Type.list () (Type.fresh_param ()), new_bindings, sol)
  | List_lit (p :: ps) ->
     let* p_ty, new_bindings, sol = type_of_pattern ctxt p new_bindings sol in
     let* new_bindings, sol = type_of_pattern_list_eq ctxt ps p_ty new_bindings sol in
     Ok (Type.list () p_ty, new_bindings, sol)
  | Constr (name, arg) -> (
    match Ctxt.find_opt name.name ctxt with
    | None -> Error (Error.unbound_constr name.meta name.name)
    | Some ty -> (
      let ty = Type_scheme.instantiate ty in
      match ty.ty, arg with
      | Adt (tps, name), None -> Ok (Type.adt () tps name, new_bindings, sol)
      | Adt (tps, _), Some _ -> Error (Error.constr_unexpected_arg name.meta name.name)
      | Fun (_, _), None -> Error (Error.constr_expected_arg name.meta name.name)
      | Fun (constr_arg_ty, ty), Some arg_pat ->
         let* arg_ty, new_bindings, sol = type_of_pattern ctxt arg_pat new_bindings sol in
         let* sol = Unifier.unify p.meta constr_arg_ty arg_ty sol in
         Ok (ty, new_bindings, sol)
      | _ -> assert false (* IMPOSSIBLE *)
    )
  )
and type_of_pattern_list
      (ctxt : Ctxt.t)
      (ps : Loc.t Pattern.t list)
      (new_bindings : Ctxt.t)
      (sol : Unifier.t)
    : (unit Type.t list * Ctxt.t * Unifier.t, Error_msg.t) result =
  match ps with
  | [] -> assert false (* IMPOSSIBLE *)
  | [p] ->
     let* t, new_bindings, sol = type_of_pattern ctxt p new_bindings sol in
     Ok ([t], new_bindings, sol)
  | p :: ps ->
     let* t, new_bindings, sol = type_of_pattern ctxt p new_bindings sol in
     let* ts, new_bindings, sol = type_of_pattern_list ctxt ps new_bindings sol in
     Ok (t :: ts, new_bindings, sol)
and type_of_pattern_list_eq
      (ctxt : Ctxt.t)
      (ps : Loc.t Pattern.t list)
      (ty : unit Type.t)
      (new_bindings: Ctxt.t)
      (sol : Unifier.t)
    : (Ctxt.t * Unifier.t, Error_msg.t) result =
  match ps with
  | [] -> Ok (new_bindings, sol)
  | p :: ps ->
     let* p_ty, new_bindings, sol = type_of_pattern ctxt p new_bindings sol in
     let* sol = Unifier.unify p.meta ty p_ty sol in
     type_of_pattern_list_eq ctxt ps ty new_bindings sol

let type_of_pattern ctxt p sol = type_of_pattern ctxt p Ctxt.empty sol
let type_of_pattern_list ctxt p sol = type_of_pattern_list ctxt p Ctxt.empty sol

let rec type_of_expr
          (ctxt : Ctxt.t)
          (e : Loc.t Expr.t)
          (sol : Unifier.t)
        : (unit Type.t * Unifier.t, Error_msg.t) result =
  match e.expr with
  | Var x ->
     let* ty =
       match Ctxt.find_opt x.name ctxt with
       | Some ty -> Ok (Type_scheme.instantiate ty)
       | None -> Error (Error.unbound_value x.meta x.name)
     in
     Ok (ty, sol)
  | If (e1, e2, e3) ->
     let* e1_ty, sol = type_of_expr ctxt e1 sol in
     let* sol = Unifier.unify e1.meta e1_ty (Type.bool ()) sol in
     let* e2_ty, sol = type_of_expr ctxt e2 sol in
     let* e3_ty, sol = type_of_expr ctxt e3 sol in
     let* sol = Unifier.unify e3.meta e2_ty e3_ty sol in
     Ok (e3_ty, sol)
  (* (\* Equivalent implementation with match-expressions *\) *)
  (* | If (e1, e2, e3) -> ( *)
  (*    match type_of_expr ctxt e1 sol with *)
  (*    | Ok (e1_ty, sol) -> ( *)
  (*      match Unifier.unify e.meta e1_ty (Type.bool ()) sol with *)
  (*      | Ok sol -> ( *)
  (*        match type_of_expr ctxt e2 sol with *)
  (*        | Ok (e2_ty, sol) -> ( *)
  (*          match type_of_expr ctxt e3 sol with *)
  (*          | Ok (e3_ty, sol) -> ( *)
  (*            match Unifier.unify e3.meta e2_ty e3_ty sol with *)
  (*            | Ok sol -> Ok (e3_ty, sol) *)
  (*            | Error e -> Error e *)
  (*          ) *)
  (*          | Error e -> Error e *)
  (*        ) *)
  (*        | Error e -> Error e *)
  (*      ) *)
  (*      | Error e -> Error e *)
  (*    ) *)
  (*    | Error e -> Error e *)
  (* ) *)
  | _ -> assert false (* TODO *)

(* SEMANTICS ================================================================ *)

module Value = struct
  module M = Map.Make (String)

  type t =
    | Unit
    | Bool of bool
    | Int of int
    | Constr of string * t option
    | List of t list
    | Tuple of t list
    | Clos of {
        env : t M.t;
        args : Loc.t Pattern.t list;
        body : Loc.t Expr.t;
        name : Loc.t Ident.t option;
      }

  let rec to_string = function
    | Unit -> "()"
    | Bool b -> string_of_bool b
    | Int n -> string_of_int n
    | Constr (name, None) -> name
    | Constr (name, Some v) ->
       let s = to_string v in
       let s =
         match v with
         | Constr (_, Some _) -> "(" ^ s ^ ")"
         | Int n when n < 0 -> "(" ^ s ^ ")"
         | _ -> s
       in
       Format.sprintf "%s %s" name s
    | List vs -> string_of_list to_string vs
    | Tuple vs -> string_of_tuple to_string vs
    | Clos _ -> "<fun>"
end

module Env = struct
  module M = Map.Make (String)

  type t = Value.t M.t

  let empty = M.empty
  let find = M.find
  let add (x : string) (v : Value.t) : t -> t = M.add x v

  let merge (c1 : t) (c2 : t) : t =
    let merger _ t1 t2 =
      match t1, t2 with
      | _, Some ty -> Some ty
      | t1, _ -> t1
    in
    SMap.merge merger c1 c2

  let to_string t =
    t
    |> M.to_list
    |> string_of_list (fun (x, v) -> "(" ^ x ^ ", " ^ Value.to_string v ^ ")")
end

exception Assert_fail of Loc.t
exception Match_fail of Loc.t
exception Div_by_zero of Loc.t

let rec matches (v : Value.t) (p : Loc.t Pattern.t) (env : Env.t) : Env.t option =
  let ( let* ) = Option.bind in
  match v, p.pattern with
  | _, Wild -> Some env
  | Unit, Unit -> Some env
  | Bool vb, Bool pv when vb = pv -> Some env
  | Int vn, Int pn when vn = pn -> Some env
  | v, Var x -> Some (Env.add x.name v env)
  | Tuple vs, Tuple ps -> matches_list vs ps env
  | v, Annot (p, _) -> matches v p env
  | List (v :: vs), Cons (p, ps) ->
     let* env = matches v p env in
     matches (List vs) ps env
  | List vs, List_lit ps -> matches_list vs ps env
  | Constr (vname, None), Constr (pname, None) when vname = pname.name -> Some env
  | Constr (vname, Some v), Constr (pname, Some p) when vname = pname.name ->
     matches v p env
  | _ -> None
and matches_list (vs : Value.t list) (ps : Loc.t Pattern.t list) (env : Env.t) : Env.t option =
  match vs, ps with
  | [], [] -> Some env
  | v :: vs, p :: ps -> (
    match matches v p env with
    | Some env -> matches_list vs ps env
    | None -> None
  )
  | _ -> None

let rec eval_expr (env : Env.t) (e : Loc.t Expr.t) : Value.t =
  match e.expr with
  | _ -> assert false (* TODO *)

(* INTERPRETER ============================================================== *)

let process_stmt
      (ctxt : Ctxt.t)
      (env : Env.t)
      (stmt : Loc.t Stmt.t)
    : (Ctxt.t * Env.t, Error_msg.t) result =
  match stmt.stmt with
  | Let {is_rec; name; args; ty_annot; binding} ->
     let e =
       Expr.let_
         stmt.meta
         is_rec
         name
         args
         ty_annot
         binding
         (Expr.var Loc.dummy name)
     in
     let* ty, sol = type_of_expr ctxt e Unifier.empty in
     let ty = Unifier.principal sol ty in
     let v = eval_expr env e in
     let _ =
       Format.printf
         "val %s : %s = %s\n"
         name.name
         (Type_scheme.to_string ty)
         (Value.to_string v)
     in
     Ok (Ctxt.add name.name ty ctxt, Env.add name.name v env)
  | Let_pat {pat; ty_annot; binding} -> (
    let* pat_ty, new_decls, sol  =
      type_of_pattern ctxt pat Unifier.empty in
    let* sol =
      match ty_annot with
      | None -> Ok sol
      | Some exp_ty ->
         let* () = check_ty (bound_adt_names ctxt) exp_ty in
         Unifier.unify pat.meta (Type.erase exp_ty) pat_ty sol
    in
    let* binding_ty, sol = type_of_expr ctxt binding sol in
    let* sol = Unifier.unify binding.meta pat_ty binding_ty sol in
    let vbinding = eval_expr env binding in
    let _ =
      if pat.pattern = Wild
      then
        let ty = Unifier.principal sol binding_ty in
        Format.printf
          "val - : %s = %s\n"
          (Type_scheme.to_string ty)
          (Value.to_string vbinding)
      else ()
    in
    let new_decls =
      SMap.map
        (fun (Forall (_, ty) : Type_scheme.t) ->
          Unifier.principal sol ty)
        new_decls
    in
    match matches vbinding pat Env.empty with
    | None -> raise (Match_fail pat.meta)
    | Some new_bindings ->
       let _ =
         SMap.iter
           (fun x (ty : Type_scheme.t) ->
             let v = Env.find x new_bindings in
             Format.printf
               "val %s : %s = %s\n"
               x
               (Type_scheme.to_string ty)
               (Value.to_string v))
           new_decls
       in
       Ok (Ctxt.merge ctxt new_decls, Env.merge env new_bindings)
  )
  | Adt {tpars; name; constrs} -> (
     let rec repeat tpars =
       let rec go a tpars =
         match tpars with
         | [] -> None
         | tp :: tpars ->
            if Ident.equal a tp
            then Some tp
            else go a tpars
       in
       match tpars with
       | [] -> None
       | a :: tpars -> (
         match go a tpars with
         | None -> repeat tpars
         | Some tp -> Some tp
       )
     in
     match repeat tpars with
     | None ->
        if List.mem_assoc name.name (bound_adt_names ctxt)
        then Error (Error.dup_type_name name.meta name.name)
        else
          let tpars_erased = List.map Ident.erase tpars in
          let tpars_params = List.map (fun x -> Type.param () x) tpars_erased in
          let name = Ident.map_meta (fun _ -> ()) name in
          let rec go ctxt (cs : (Loc.t Ident.t * Loc.t Type.t option) list) =
            match cs with
            | [] -> Ok ctxt
            | (cons_name, None) :: cs ->
               let ctxt =
                 Ctxt.add
                   cons_name.name
                   (Type_scheme.mk tpars_erased (Type.adt () tpars_params name))
                   ctxt
               in go ctxt cs
            | (cons_name, Some ty) :: cs ->
               match
                 List.find_opt
                   (fun x -> not (List.exists (Ident.equal x) tpars))
                   (Type.fv ty)
               with
               | None ->
                  let* () =
                    check_ty
                      ((name.name, List.length tpars_erased) :: bound_adt_names ctxt)
                      ty
                  in
                  let ctxt =
                    Ctxt.add
                      cons_name.name
                      (Type_scheme.mk
                         tpars_erased
                         (Type.fun_ ()
                            [Type.erase ty]
                            (Type.adt () tpars_params name)))
                      ctxt
                  in
                  go ctxt cs
               | Some x -> Error (Error.unbound_ty_var x.meta x.name)
          in
          let* ctxt = go ctxt constrs in
          let _ =
            Format.printf
              "type %s\n"
              Type.(to_string (adt () tpars_params name))
          in
          Ok (ctxt, env)
     | Some tp -> Error (Error.type_param_occurs_several_times tp.meta)
  )

let rec process_stmts ctxt env prog =
  match prog with
  | [] -> Ok (ctxt, env)
  | s :: ss ->
     let* ctxt, env = process_stmt ctxt env s in
     process_stmts ctxt env ss

let interp ~(filename : string) : (Ctxt.t * Env.t, Error_msg.t) result =
  let* prog = Syntax.parse ~filename in
  match process_stmts Ctxt.empty Env.empty prog with
  | v -> v
  | exception Assert_fail loc -> Error (Error_msg.mk loc "(Exception) Assert_fail")
  | exception Div_by_zero loc -> Error (Error_msg.mk loc "(Exception) Div_by_zero")
  | exception Match_fail loc -> Error (Error_msg.mk loc "(Exception) Match_fail")

(* TESTING FUNCTIONS ======================================================== *)

module Test = struct
  let prog s =
    List.map
      (Stmt.map_meta (fun _ -> ()))
      (Parser.prog Lexer.read (Lexing.from_string s))

  let ty s =
    match prog ("let _ : " ^ s ^ " = ()") with
    | [{stmt=Let_pat {ty_annot=Some t;_};_}] -> Type.map_meta (fun _ -> ()) t
    | _ -> failwith "Test.ty"

  let pat s =
    match prog ("let " ^ s ^ " = ()") with
    | [{stmt=Let_pat {pat=p;_};_}] -> Pattern.map_meta (fun _ -> ()) p
    | _ -> failwith "Test.pat"

  let expr s =
    match prog ("let _ = " ^ s) with
    | [{stmt=Let_pat {binding=e;_};_}] -> Expr.map_meta (fun _ -> ()) e
    | _ -> failwith "Test.expr"

  let stmt s =
    match prog s with
    | [s] -> Stmt.map_meta (fun _ -> ()) s
    | _ -> failwith "Test.stmt"

  let ctxt l =
    let rec go acc = function
      | [] -> acc
      | (x, s) :: l -> go (Ctxt.add x (ty s) acc) l
    in go Ctxt.empty l

  let unifier l = List.map (fun (x, a) -> x, ty a) l
end
