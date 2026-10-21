
let is_ws = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

module Sexpr = struct
  module Token = struct
    type t = Lpar | Rpar | Word of string

    let list_of_string s =
      let at_end_of_word i =
        i >= String.length s
        || match s.[i] with | '(' | ')' -> true | c -> is_ws c
      in
      let rec go acc i =
        if i >= String.length s
        then List.rev acc
        else
          match s.[i] with
          | '(' -> go (Lpar :: acc) (i + 1)
          | ')' -> go (Rpar :: acc) (i + 1)
          | c ->
             if is_ws c
             then go acc (i + 1)
             else go' acc i 1
      and go' acc start len =
        let i = start + len in
        if at_end_of_word i
        then go (Word (String.sub s start len) :: acc) i
        else go' acc start (len + 1)
      in go [] 0
  end

  type t =
    | Atom of string
    | List of t list

  let of_tokens ts =
    let rec go ts =
      match ts with
      | Token.Lpar :: ts ->
        begin
          match go' ts with
          | es, Token.Rpar :: ts -> Some (List es, ts)
          | _ -> None
        end
      | Word w :: ts -> Some (Atom w, ts)
      | _ -> None
    and go' ts =
      match go ts with
      | None -> [], ts
      | Some (e, ts) ->
         let es, ts = go' ts in
         e :: es , ts
    in
    match go ts with
    | Some (e, []) -> Some e
    | _ -> None

  let of_string s = s |> Token.list_of_string |> of_tokens
end

module Expr = struct
  type op = Add | Sub | Mul
  type t =
    | Bop of op * t * t
    | Int of int

  let ( let* ) = Option.bind

  let rec of_sexpr (e : Sexpr.t) =
    let bop_of_word = function
      | "+" -> Some Add
      | "-" -> Some Sub
      | "*" -> Some Mul
      | _ -> None
    in
    match e with
    | List [Atom op; e1; e2] ->
       let* op = bop_of_word op in
       let* e1 = of_sexpr e1 in
       let* e2 = of_sexpr e2 in
       Some (Bop (op, e1, e2))
    | Atom n ->
       let* n = int_of_string_opt n in
       Some (Int n)
    | _ -> None

  let of_string s =
    let* se = Sexpr.of_string s in
    of_sexpr se
end
