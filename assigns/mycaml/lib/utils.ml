let ( let* ) = Result.bind

module SMap = Map.Make (String)

module Loc = struct
  type t =
    {
      first : Lexing.position;
      last : Lexing.position;
    }

  let mk (first, last) = { first; last }

  let first t = t.first
  let last t = t.last

  let dummy = mk (Lexing.dummy_pos, Lexing.dummy_pos)
end

let split_last l =
  let rec go acc = function
    | [] -> assert false
    | [x] -> List.rev acc, x
    | x :: xs -> go (x :: acc) xs
  in go [] l

let rec nub l =
  match l with
  | [] -> []
  | x :: xs -> x :: List.filter ((<>) x) (nub xs)

let string_of_list f l =
  "[" ^ String.concat "; " (List.map f l) ^ "]"

let string_of_tuple f l =
  "(" ^ String.concat ", " (List.map f l) ^ ")"
