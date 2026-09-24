
let is_ws c = c = ' ' || c = '\n' || c = '\r' || c = '\t' || c = '\012'

let split_by_ws (s : string) : string list =
  let _ = s in
  assert false

type dir = N | S | E | W

let dist (dirs : dir list) : float =
  let _ = dirs in
  assert false

type int_or_string
  = Int of int
  | String of string

type int_list_or_string_list
  = Int_list of int list
  | String_list of string list

let convert (l : int_or_string list) : int_list_or_string_list list =
  let _ = l in
  assert false

type 'a tree
  = Empty
  | Node of 'a * 'a tree * 'a tree

let rec insert (x : 'a) (t : 'a tree) : 'a tree =
  let _ = x, t in
  assert false

let rec flatten (t : 'a tree) : 'a list =
  let _ = t in
  assert false

let rec sort (l : 'a list) : 'a list =
  let _ = l in
  assert false
