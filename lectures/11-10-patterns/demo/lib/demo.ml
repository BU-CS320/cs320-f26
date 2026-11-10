
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

let eval (e : expr) : expr = ignore e; Num 0

let interp (s : string) : (expr, string) result =
  match parse s with
  | Some expr -> Ok (eval expr) (* TODO: Type check *)
  | None -> Error "whoops"
