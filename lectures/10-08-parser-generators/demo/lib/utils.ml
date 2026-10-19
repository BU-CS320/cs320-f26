type expr =
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Num of int
  | Var of string
  | Let of string * expr * expr   (* let <var> = <expr> in <expr> *)