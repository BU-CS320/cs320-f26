open Utils

module Ident = struct
  type 'a t =
    {
      meta : 'a;
      name : string;
    }

  let mk meta name = { meta; name }
  let map_meta f t = { t with meta = f t.meta }
  let erase i = map_meta (fun _ -> ()) i
  let equal i1 i2 = i1.name = i2.name
end

module Type = struct
  type 'a ty =
    | Unit
    | Bool
    | Int
    | Param of 'a Ident.t
    | Fun of 'a t * 'a t
    | Tuple of 'a t list
    | List of 'a t
    | Adt of 'a t list * 'a Ident.t
  and 'a t =
    {
      meta : 'a;
      ty : 'a ty;
    }

  let rec to_string t =
    let to_string_parens t =
      match t.ty with
      | Fun _ | Tuple _ -> "(" ^ to_string t ^ ")"
      | _ -> to_string t
    in
    match t.ty with
    | Unit -> "unit"
    | Bool -> "bool"
    | Int -> "int"
    | Param a -> a.name
    | Fun (in_ty, out_ty) ->
       to_string_parens in_ty ^ " -> " ^ to_string out_ty
    | Tuple tys ->
       String.concat " * " (List.map to_string_parens tys)
    | List ty -> to_string_parens ty ^ " list"
    | Adt ([], n) -> n.name
    | Adt ([ty], n) -> to_string_parens ty ^ " " ^ n.name
    | Adt (tys, n) ->
       "("
       ^ String.concat ", " (List.map to_string tys)
       ^ ") "
       ^ n.name

  let mk meta ty = { meta; ty }
  let unit meta = mk meta Unit
  let bool meta = mk meta Bool
  let int meta = mk meta Int
  let param meta a = mk meta (Param a)
  let fun_ meta in_ts out_t =
    List.fold_right (fun in_t acc -> mk meta (Fun (in_t, acc))) in_ts out_t
  let fun_of_list meta ts =
    let in_ts, out_t = Utils.split_last ts in
    fun_ meta in_ts out_t
  let tuple meta ts = mk meta (Tuple ts)
  let list meta t = mk meta (List t)
  let adt meta tps name = mk meta (Adt (tps, name))

  let map_meta f t =
    let rec go t =
      let m = f t.meta in
      match t.ty with
      | Unit -> unit m
      | Bool -> bool m
      | Int -> int m
      | Param a -> param m (Ident.map_meta f a)
      | Fun (in_t, out_t) -> fun_ m [go in_t] (go out_t)
      | Tuple ts -> tuple m (List.map go ts)
      | List t -> list m (go t)
      | Adt (tps, name) -> adt m (List.map go tps) (Ident.map_meta f name)
    in go t

  let erase t = map_meta (fun _ -> ()) t

  let equal t1 t2 = map_meta (fun _ -> ()) t1 = map_meta (fun _ -> ()) t2

  let subst t1 a t2 =
    let m = t2.meta in
    let rec go t2 =
      match t2.ty with
      | Unit | Bool | Int -> t2
      | Param b -> if b.name = a then t1 else t2
      | Fun (t1, t2) -> fun_ m [go t1] (go t2)
      | Tuple ts -> tuple m (List.map go ts)
      | List t -> list m (go t)
      | Adt (tps, name) -> adt m (List.map go tps) name
    in go t2

  let rec fv ty =
    let rec go ty =
      match ty.ty with
      | Unit | Bool | Int -> []
      | Param a -> [a]
      | Fun (in_ty, out_ty) -> fv in_ty @ fv out_ty
      | Tuple tys -> List.concat_map fv tys
      | List ty -> fv ty
      | Adt (tys, _) -> List.concat_map fv tys
    in nub (go ty)

  let normalize ty =
    let fvs = fv ty in
    let letters =
      List.init
        (List.length fvs)
        (fun i -> "\'" ^ String.make 1 (char_of_int (97 + i)))
    in
    let fvs_letters = List.combine fvs letters in
    List.fold_left
      (fun acc ((x : 'a Ident.t), a) -> subst (param x.meta (Ident.mk x.meta a)) x.name acc)
      ty
      fvs_letters

  let fresh_param =
    let n = ref 0 in
    let go () =
      n := !n + 1;
      param () (Ident.mk () ("\'" ^ string_of_int !n))
    in go
end

module Pattern = struct
  type 'a pattern =
    | Wild
    | Unit
    | Bool of bool
    | Int of int
    | Var of 'a Ident.t
    | Tuple of 'a t list
    | Annot of 'a t * 'a Type.t
    | Cons of 'a t * 'a t
    | List_lit of 'a t list
    | Constr of 'a Ident.t * 'a t option
  and 'a t =
    {
      meta : 'a;
      pattern : 'a pattern;
    }

  let mk meta pattern = { meta; pattern }
  let wild meta = mk meta Wild
  let unit meta = mk meta Unit
  let true_ meta = mk meta (Bool true)
  let false_ meta = mk meta (Bool false)
  let int meta n = mk meta (Int n)
  let var meta x = mk meta (Var x)
  let tuple meta ps = mk meta (Tuple ps)
  let annot meta p ty = mk meta (Annot (p, ty))
  let constr meta name arg = mk meta (Constr (name, arg))
  let cons meta x xs = mk meta (Cons (x, xs))
  let list_lit meta xs = mk meta (List_lit xs)

  let map_meta f t =
    let rec go t =
      let m = f t.meta in
      match t.pattern with
      | Wild -> wild m
      | Unit -> unit m
      | Bool b -> if b then true_ m else false_ m
      | Int n -> int m n
      | Var x -> var m (Ident.map_meta f x)
      | Tuple ps -> tuple m (List.map go ps)
      | Annot (p, t) -> annot m (go p) (Type.map_meta f t)
      | Constr (name, arg) -> constr m (Ident.map_meta f name) (Option.map go arg)
      | Cons (p1, p2) -> cons m (go p1) (go p2)
      | List_lit ps -> list_lit m (List.map go ps)
    in go t
end

module Bop = struct
  type bop =
    | Add | Sub | Mul | Div | Mod
    | And | Or
    | Cons
    | Eq | Neq | Lt | Lte | Gt | Gte

  type 'a t =
    {
      meta : 'a;
      bop : bop;
    }

  let mk meta bop = { meta; bop }
  let add meta = mk meta Add
  let sub meta = mk meta Sub
  let mul meta = mk meta Mul
  let div meta = mk meta Div
  let mod_ meta = mk meta Mod
  let and_ meta = mk meta And
  let or_ meta = mk meta Or
  let cons meta = mk meta Cons
  let eq meta = mk meta Eq
  let neq meta = mk meta Neq
  let lt meta = mk meta Lt
  let lte meta = mk meta Lte
  let gt meta = mk meta Gt
  let gte meta = mk meta Gte

  let map_meta f t = { t with meta = f t.meta }
end

module Expr = struct
  type 'a expr =
    | Unit
    | Bool of bool
    | Int of int
    | Var of 'a Ident.t
    | Negate of 'a t
    | Bop of 'a Bop.t * 'a t * 'a t
    | If of 'a t * 'a t * 'a t
    | Tuple of 'a t list
    | Assert of 'a t
    | List_lit of 'a t list
    | Constr of 'a Ident.t * 'a t option
    | Fun of 'a Pattern.t list * 'a t
    | App of 'a t * 'a t list
    | Let of
        {
          is_rec : bool;
          name : 'a Ident.t;
          args : 'a Pattern.t list;
          ty_annot : 'a Type.t option;
          binding : 'a t;
          body : 'a t;
        }
    | Let_pat of
        {
          pat : 'a Pattern.t;
          ty_annot : 'a Type.t option;
          binding : 'a t;
          body : 'a t;
        }
    | Match of 'a t * ('a Pattern.t * 'a t) list
  and 'a t =
    {
      meta : 'a;
      expr : 'a expr;
    }

  let mk meta expr = { meta; expr }
  let unit meta = mk meta Unit
  let true_ meta = mk meta (Bool true)
  let false_ meta = mk meta (Bool false)
  let int meta n = mk meta (Int n)
  let var meta x = mk meta (Var x)
  let negate meta e = mk meta (Negate e)
  let bop meta e1 op e2 = mk meta (Bop (op, e1, e2))
  let if_ meta e1 e2 e3 = mk meta (If (e1, e2, e3))
  let tuple meta es = mk meta (Tuple es)
  let assert_ meta e = mk meta (Assert e)
  let constr meta name arg = mk meta (Constr (name, arg))
  let fun_ meta args body = mk meta (Fun (args, body))
  let app meta e es = mk meta (App (e, es))
  let let_ meta is_rec name args ty_annot binding body =
    mk meta (Let { is_rec; name; args; ty_annot; binding; body })
  let let_pat meta pat ty_annot binding body =
    mk meta (Let_pat {pat; ty_annot; binding; body })
  let match_ meta e arms = mk meta (Match (e, arms))
  let list_lit meta es = mk meta (List_lit es)

  let map_meta f t =
    let rec go t =
      let m = f t.meta in
      match t.expr with
      | Unit -> unit m
      | Bool b -> if b then true_ m else false_ m
      | Int n -> int m n
      | Var x -> var m (Ident.map_meta f x)
      | Negate e -> negate m (go e)
      | Bop (op, e1, e2) -> bop m (go e1) (Bop.map_meta f op) (go e2)
      | If (e1, e2, e3) -> if_ m (go e1) (go e2) (go e3)
      | Tuple es -> tuple m (List.map go es)
      | Assert e -> assert_ m (go e)
      | Constr (name, arg) -> constr m (Ident.map_meta f name) (Option.map go arg)
      | Fun (args, body) -> fun_ m (List.map (Pattern.map_meta f) args) (go body)
      | App (e, es) -> app m (go e) (List.map go es)
      | Let {is_rec; name; args; ty_annot; binding; body} ->
         let_ m
           is_rec
           (Ident.map_meta f name)
           (List.map (Pattern.map_meta f) args)
           (Option.map (Type.map_meta f) ty_annot)
           (go binding)
           (go body)
      | Let_pat {pat; ty_annot; binding; body} ->
         let_pat m
           (Pattern.map_meta f pat)
           (Option.map (Type.map_meta f) ty_annot)
           (go binding)
           (go body)
      | Match (e, arms) ->
         match_ m
           (go e)
           (List.map (fun (p, e) -> Pattern.map_meta f p, go e) arms)
      | List_lit es -> list_lit m (List.map go es)
    in go t
end

module Stmt = struct
  type 'a stmt =
    | Let of
        {
          is_rec : bool;
          name : 'a Ident.t;
          args : 'a Pattern.t list;
          ty_annot : 'a Type.t option;
          binding : 'a Expr.t;
        }
    | Let_pat of
        {
          pat : 'a Pattern.t;
          ty_annot : 'a Type.t option;
          binding : 'a Expr.t;
        }
    | Adt of
        {
          tpars : 'a Ident.t list;
          name : 'a Ident.t;
          constrs : ('a Ident.t * 'a Type.t option) list
        }

  type 'a t =
    {
      meta : 'a;
      stmt : 'a stmt;
    }

  let mk meta stmt = { meta; stmt }
  let let_ meta is_rec name args ty_annot binding =
    mk meta (Let { is_rec; name; args; ty_annot; binding })
  let let_pat meta pat ty_annot binding =
    mk meta (Let_pat {pat; ty_annot; binding })
  let adt meta tpars name constrs =
    mk meta (Adt { tpars; name; constrs })

  let map_meta f t =
    let m = f t.meta in
    let rec go t =
      match t.stmt with
      | Let {is_rec; name; args; ty_annot; binding} ->
         let_ m
           is_rec
           (Ident.map_meta f name)
           (List.map (Pattern.map_meta f) args)
           (Option.map (Type.map_meta f) ty_annot)
           (Expr.map_meta f binding)
      | Let_pat {pat; ty_annot; binding} ->
         let_pat m
           (Pattern.map_meta f pat)
           (Option.map (Type.map_meta f) ty_annot)
           (Expr.map_meta f binding)
      | Adt {tpars; name; constrs} ->
         adt m
           (List.map (Ident.map_meta f) tpars)
           (Ident.map_meta f name)
           (List.map
              (fun (x, ty) ->
                (Ident.map_meta f x, Option.map (Type.map_meta f) ty))
              constrs)
    in go t
end

module Prog = struct
  type 'a t = 'a Stmt.t list

  let mk stmts = stmts
  let map_meta f t = List.map (Stmt.map_meta f) t
end
