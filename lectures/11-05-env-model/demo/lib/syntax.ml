type expr =
  | Var of string
  | Num of int
  | Fun of string * expr
  | Add of expr * expr
  | Eq of expr * expr
  | If of expr * expr * expr
  | App of expr * expr
  | Let of string * expr * expr
