
let is_lower c = c >= 'a' && c <= 'z'

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
  type t =
    | Fun of string * t
    | App of t * t
    | Var of string

  let to_string e =
    let rec go = function
      | Fun (x, e) -> Format.sprintf "(FUN %s %s)" x (go e)
      | App (e1, e2) -> Format.sprintf "(%s %s)" (go e1) (go e2)
      | Var x -> x
    in go e

  let ( let* ) = Option.bind

  let of_sexpr : Sexpr.t -> t option = assert false

  let of_string s =
    let* se = Sexpr.of_string s in
    of_sexpr se
end
