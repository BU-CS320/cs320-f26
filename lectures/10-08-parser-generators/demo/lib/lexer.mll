(* {
open Parser
}

let whitespace = [' ' '\n' '\r' '\t']
let num = '-'?['0'-'9']+
let var = ['a'-'z']+

rule read =
  parse
  | "let"       { LET }
  | "="         { EQ }
  | "in"        { IN }
  | "+"         { PLUS }
  | "-"         { MINUS }
  | "*"         { STAR }
  | "/"         { SLASH }
  | "("         { LPAREN }
  | ")"         { RPAREN }
  | var         { IDENT (Lexing.lexeme lexbuf) }
  | num         { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | whitespace  { read lexbuf }
  | eof         { EOF } *)


{
  open Parser
}

let whitespace = [' ' '\n' '\t' '\r' '\012']
let num = '-'?['0'-'9']+
let var = ['a'-'z']+

rule read =
  parse
  | "let" { LET }
  | "="   { EQ }
  | "in"  { IN }
  | "+"   { PLUS }
  | "-"   { MINUS }
  | "*"   { STAR }
  | "/"   { SLASH }
  | "("   { LPAREN }
  | ")"   { RPAREN }
  | num   { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | var   { IDENT (Lexing.lexeme lexbuf) }
  | whitespace {read lexbuf}
  | eof   { EOF }
