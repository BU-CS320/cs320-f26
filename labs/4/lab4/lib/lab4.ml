
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

  let of_tokens ts = ignore ts; assert false

  let of_string s = s |> Token.list_of_string |> of_tokens
end

module Expr = struct
  type op = Add | Sub | Mul
  type t =
    | Bop of op * t * t
    | Int of int

  let ( let* ) = Option.bind

  let rec of_sexpr (e : Sexpr.t) = ignore e; assert false

  let of_string s =
    let* se = Sexpr.of_string s in
    of_sexpr se
end
