
type ty =
  | TInt
  | TBool
  | TFun of ty * ty
  | TPar of string

type ty_scheme = string list * ty

type expr =
  | Var of string
  | Num of int
  | Bool of bool
  | Fun of string * expr
  | App of expr * expr
  | Add of expr * expr
  | Eq of expr * expr
  | If of expr * expr * expr
  | Let of string * expr * expr

type prog = (string * expr) list

module Env = Map.Make(String)

type env = value Env.t
and value =
  | VBool of bool
  | VNum of int
  | VClos of string * expr * env
