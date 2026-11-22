{
open Utils
open Parser

exception Unknown_token of Loc.t * string
exception Unclosed_comment of Loc.t

let loc buf = Loc.mk Lexing.(lexeme_start_p buf, lexeme_end_p buf)
}

let int_lit    = ['0'-'9']+

let var_id     = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
let constr_id  = ['A'-'Z']     ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
let tpar_id    = '\''          ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

let whitespace = [' ' '\t' '\r']+

rule read =
  parse
  | "type"       { TYPE                                                     }
  | "of"         { OF                                                       }
  | "let"        { LET                                                      }
  | "rec"        { REC                                                      }
  | "in"         { IN                                                       }
  | "if"         { IF                                                       }
  | "then"       { THEN                                                     }
  | "else"       { ELSE                                                     }
  | "fun"        { FUN                                                      }
  | "match"      { MATCH                                                    }
  | "with"       { WITH                                                     }
  | "assert"     { ASSERT                                                   }
  | "mod"        { MOD                                                      }

  | "list"       { LIST_TY                                                  }
  | "unit"       { UNIT_TY                                                  }
  | "bool"       { BOOL_TY                                                  }
  | "int"        { INT_TY                                                   }

  | "true"       { TRUE                                                     }
  | "false"      { FALSE                                                    }

  | "->"         { ARROW                                                    }

  | "&&"         { AND                                                      }
  | "||"         { OR                                                       }

  | "::"         { CONS                                                     }

  | "<="         { LTE                                                      }
  | ">="         { GTE                                                      }
  | "<>"         { NEQ                                                      }

  | "(*"         { read_comment 0 lexbuf                                    }

  | "+"          { PLUS                                                     }
  | "-"          { MINUS                                                    }
  | "*"          { STAR                                                     }
  | "/"          { SLASH                                                    }
  | "="          { EQ                                                       }
  | "<"          { LT                                                       }
  | ">"          { GT                                                       }
  | "("          { LPAREN                                                   }
  | ")"          { RPAREN                                                   }
  | "["          { LBRACKET                                                 }
  | "]"          { RBRACKET                                                 }
  | "|"          { ALT                                                      }
  | ":"          { COLON                                                    }
  | ";"          { SEMICOLON                                                }
  | ","          { COMMA                                                    }

  | int_lit      { INT_LIT (int_of_string (Lexing.lexeme lexbuf))           }

  | '_'          { WILD                                                     }
  | tpar_id      { TPAR_ID (Lexing.lexeme lexbuf)                           }
  | var_id       { VAR_ID (Lexing.lexeme lexbuf)                            }
  | constr_id    { CONSTR_ID (Lexing.lexeme lexbuf)                         }

  | whitespace   { read lexbuf }
  | '\n'         { Lexing.new_line lexbuf; read lexbuf }
  | _            { raise (Unknown_token (loc lexbuf, Lexing.lexeme lexbuf)) }

  | eof          { EOF                                                      }

and read_comment depth =
  parse
  | "(*"         { read_comment (depth + 1) lexbuf                          }
  | "*)"         { if depth = 0
                   then read lexbuf
                   else read_comment (depth - 1) lexbuf                     }
  | '\n'         { Lexing.new_line lexbuf; read_comment depth lexbuf        }
  | [^ '(' '*']+ { read_comment depth lexbuf                                }
  | ['(' '*']    { read_comment depth lexbuf                                }
  | _            { raise (Unclosed_comment (loc lexbuf))                    }
