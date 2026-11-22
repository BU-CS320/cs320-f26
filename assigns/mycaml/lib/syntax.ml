(*
    Borrowing from
    https://gitlab.inria.fr/fpottier/menhir/-/blob/master/demos/calc-syntax-errors
*)

open Utils

module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil
module I = Parser.MenhirInterpreter

let env checkpoint =
  match checkpoint with
  | I.HandlingError env -> env
  | _ -> assert false

let state checkpoint : int =
  match I.top (env checkpoint) with
  | Some (I.Element (s, _, _, _)) -> I.number s
  | None -> 0

let fail _text buffer (checkpoint : _ I.checkpoint) =
  let position = Loc.mk (E.last buffer) in
  let state = state checkpoint in
  Error (Error_msg.mk position (String.trim (SyntaxError.message state)))

let parse ~filename =
  let text, lexbuf = L.read filename in
  let lexbuf = L.init filename lexbuf in
  let supplier = I.lexer_lexbuf_to_supplier Lexer.read lexbuf in
  let buffer, supplier = E.wrap_supplier supplier in
  let checkpoint = Parser.Incremental.prog lexbuf.lex_curr_p in
  match I.loop_handle Result.ok (fail text buffer) supplier checkpoint with
  | prog -> prog
  | exception Lexer.Unclosed_comment pos -> Error (Error_msg.mk pos "Unclosed comment")
  | exception Lexer.Unknown_token (pos, s) -> Error (Error_msg.mk pos (Printf.sprintf "Unknown token: %s" s))
