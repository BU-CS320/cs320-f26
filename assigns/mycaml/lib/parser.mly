%{
    open Utils
    open Ast
%}

(* KEYWORDS ================================================================================================================================================================= *)

%token          TYPE         "type"
%token          OF           "of"
%token          LET          "let"
%token          REC          "rec"
%token          IN           "in"
%token          IF           "if"
%token          THEN         "then"
%token          ELSE         "else"
%token          FUN          "fun"
%token          MATCH        "match"
%token          WITH         "with"
%token          ASSERT       "assert"

%token          LIST_TY      "list"
%token          UNIT_TY      "unit"
%token          BOOL_TY      "bool"
%token          INT_TY       "int"

(* SYMBOLS ================================================================================================================================================================== *)

%token          ARROW        "->"

%token          PLUS         "+"
%token          MINUS        "-"
%token          STAR         "*"
%token          SLASH        "/"
%token          MOD          "mod"
%token          AND          "&&"
%token          OR           "||"
%token          CONS         "::"
%token          NEQ          "<>"
%token          LTE          "<="
%token          LT           "<"
%token          GTE          ">="
%token          GT           ">"
%token          EQ           "="

%token          LPAREN       "("
%token          RPAREN       ")"
%token          LBRACKET     "["
%token          RBRACKET     "]"
%token          ALT          "|"
%token          COLON        ":"
%token          SEMICOLON    ";"
%token          COMMA        ","

(* LITERALS ================================================================================================================================================================= *)

%token          TRUE         "true"
%token          FALSE        "false"
%token<int>     INT_LIT

(* IDENTIFIERS ============================================================================================================================================================== *)

%token<string>  VAR_ID
%token<string>  CONSTR_ID
%token<string>  TPAR_ID
%token          WILD         "_"

(* PRECEDENCE =============================================================================================================================================================== *)

%right          OR
%right          AND
%left           LT LTE GT GTE EQ NEQ
%right          CONS
%left           PLUS MINUS
%left           STAR SLASH MOD

(* GRAMMAR ================================================================================================================================================================== *)

%token          EOF

%start<Utils.Loc.t Ast.Prog.t> prog

%%

delim(d, x):
  | d x=x                                                                                     {                                 x                                             }

nonempty_semi_list(x):
  | x=x                                                                                       {                                [x]                                            }
  | x=x ";"                                                                                   {                                [x]                                            }
  | x=x ";" xs=nonempty_semi_list(x)                                                          {                                 x :: xs                                       }

semi_list(x):
  | es=nonempty_semi_list(x)?                                                                 {                                 Option.value es ~default:[]                   }

list_lit(x):
  | "[" xs=semi_list(x) "]"                                                                   {                                 xs                                            }

var:
  | x=VAR_ID                                                                                  { Ident.mk         (Loc.mk $loc)  x                                             }

tpar:
  | a=TPAR_ID                                                                                 { Ident.mk         (Loc.mk $loc)  a                                             }

constr:
  | c=CONSTR_ID                                                                               { Ident.mk         (Loc.mk $loc)  c                                             }

prog:
  | stmts=stmt* EOF                                                                           { Prog.mk                         stmts                                         }

stmt:
  | "let"       p=pattern                                     "=" binding=expr                { Stmt.let_pat     (Loc.mk $loc)        p      None            binding          }
  | "let"       p=pattern3           ty_annot=delim(":", ty)  "=" binding=expr                { Stmt.let_pat     (Loc.mk $loc)        p      (Some ty_annot) binding          }
  | "let"       x=var args=pattern3+ ty_annot=delim(":", ty)? "=" binding=expr                { Stmt.let_        (Loc.mk $loc)  false x args ty_annot        binding          }
  | "let" "rec" x=var args=pattern3+ ty_annot=delim(":", ty)? "=" binding=expr                { Stmt.let_        (Loc.mk $loc)  true  x args ty_annot        binding          }
  | "type" tpars=tpars? name=var "=" constrs=constr_decl+                                     { Stmt.adt         (Loc.mk $loc) (Option.value ~default:[] tpars) name constrs  }

tpars:
  | p=tpar                                                                                    {                                [p]                                            }
  | "(" p=tpar ps=delim(",", tpar)+ ")"                                                       {                                 p :: ps                                       }

constr_decl:
  | "|" name=constr                                                                           {                                 name, None                                    }
  | "|" name=constr "of" t=ty                                                                 {                                 name, Some t                                  }

(* PATTERNS ================================================================================================================================================================ *)

pattern:
  | p=pattern2 ps=delim(",", pattern2)+                                                       { Pattern.tuple    (Loc.mk $loc) (p :: ps)                                      }
  | p=pattern2                                                                                {                                 p                                             }

pattern2:
  | p1=pattern2 "::" p2=pattern2                                                              { Pattern.cons     (Loc.mk $loc)  p1 p2                                         }
  | c=constr arg=pattern3                                                                     { Pattern.constr   (Loc.mk $loc)  c (Some arg)                                  }
  | p=pattern3                                                                                {                                 p                                             }

pattern3:
  | "_"                                                                                       { Pattern.wild     (Loc.mk $loc)                                                }
  | "(" ")"                                                                                   { Pattern.unit     (Loc.mk $loc)                                                }
  | "true"                                                                                    { Pattern.true_    (Loc.mk $loc)                                                }
  | "false"                                                                                   { Pattern.false_   (Loc.mk $loc)                                                }
  | n=INT_LIT                                                                                 { Pattern.int      (Loc.mk $loc)  n                                             }
  | x=var                                                                                     { Pattern.var      (Loc.mk $loc)  x                                             }
  | "(" p=pattern ":" ty=ty ")"                                                               { Pattern.annot    (Loc.mk $loc)  p  ty                                         }
  | ps=list_lit(pattern)                                                                      { Pattern.list_lit (Loc.mk $loc)  ps                                            }
  | c=constr                                                                                  { Pattern.constr   (Loc.mk $loc)  c  None                                       }
  | "(" p=pattern ")"                                                                         {                                 p                                             }

(* TYPES =================================================================================================================================================================== *)

ty:
  | t=ty2 ts=delim("->", ty2)+                                                                { Type.fun_of_list (Loc.mk $loc) (t :: ts)                                      }
  | t=ty2                                                                                     {                                 t                                             }

ty2:
  | t=ty3 ts=delim("*", ty3)+                                                                 { Type.tuple       (Loc.mk $loc) (t :: ts)                                      }
  | t=ty3                                                                                     {                                 t                                             }

ty3:
  | "unit"                                                                                    { Type.unit        (Loc.mk $loc)                                                }
  | "bool"                                                                                    { Type.bool        (Loc.mk $loc)                                                }
  | "int"                                                                                     { Type.int         (Loc.mk $loc)                                                }
  | t=ty3 "list"                                                                              { Type.list        (Loc.mk $loc)  t                                             }
  | name=var                                                                                  { Type.adt         (Loc.mk $loc) []        name                                 }
  | t=ty3 name=var                                                                            { Type.adt         (Loc.mk $loc) [t]       name                                 }
  | "(" t=ty ts=delim(",", ty)+ ")" name=var                                                  { Type.adt         (Loc.mk $loc) (t :: ts) name                                 }
  | a=tpar                                                                                    { Type.param       (Loc.mk $loc)  a                                             }
  | "(" ty=ty ")"                                                                             {                                 ty                                            }

(* EXPRESSIONS ============================================================================================================================================================= *)

expr_(last):
  | "let"       p=pattern                                     "=" binding=expr "in" body=last { Expr.let_pat      (Loc.mk $loc)        p      None            binding body    }
  | "let"       p=pattern3           ty_annot=delim(":", ty)  "=" binding=expr "in" body=last { Expr.let_pat      (Loc.mk $loc)        p      (Some ty_annot) binding body    }
  | "let"       x=var args=pattern3+ ty_annot=delim(":", ty)? "=" binding=expr "in" body=last { Expr.let_         (Loc.mk $loc)  false x args ty_annot        binding body    }
  | "let" "rec" x=var args=pattern3+ ty_annot=delim(":", ty)? "=" binding=expr "in" body=last { Expr.let_         (Loc.mk $loc)  true  x args ty_annot        binding body    }
  | "if" e1=expr "then" e2=expr "else" e3=last                                                { Expr.if_          (Loc.mk $loc)  e1 e2 e3                                     }
  | "fun" xs=pattern3+ "->" e=last                                                            { Expr.fun_         (Loc.mk $loc)  xs e                                         }

expr:
  | e=expr_(expr)                                                                             {                                  e                                            }
  | "match" e=expr "with" cs=case+                                                            { Expr.match_       (Loc.mk $loc)  e cs                                         }
  | e=expr2                                                                                   {                                  e                                            }

case:
  | "|" p=pattern "->" e=expr_no_match                                                        {                                 (p, e)                                        }

expr_no_match:
  | e=expr_(expr_no_match)                                                                    {                                  e                                            }
  | e=expr2                                                                                   {                                  e                                            }

expr2:
  | e=expr3 es=delim(",", expr3)+                                                             { Expr.tuple        (Loc.mk $loc) (e :: es)                                     }
  | e=expr3                                                                                   {                                  e                                            }

expr3:
  | e1=expr3 op=bop  e2=expr3                                                                 { Expr.bop          (Loc.mk $loc)  e1 op e2                                     }
  | "assert"          e=expr4                                                                 { Expr.assert_      (Loc.mk $loc)  e                                            }
  | "-"               e=expr4                                                                 { Expr.negate       (Loc.mk $loc)  e                                            }
  | c=constr        arg=arg?                                                                  { Expr.constr       (Loc.mk $loc)  c arg                                        }
  | e=expr4          es=arg+                                                                  { Expr.app          (Loc.mk $loc)  e  es                                        }
  | e=expr4                                                                                   {                                  e                                            }

arg:
  | e=expr4                                                                                   {                                  e                                            }
  | c=constr                                                                                  { Expr.constr       (Loc.mk $loc)  c  None                                      }

expr4:
  | "(" ")"                                                                                   { Expr.unit         (Loc.mk $loc)                                               }
  | "true"                                                                                    { Expr.true_        (Loc.mk $loc)                                               }
  | "false"                                                                                   { Expr.false_       (Loc.mk $loc)                                               }
  | n=INT_LIT                                                                                 { Expr.int          (Loc.mk $loc)  n                                            }
  | l=list_lit(expr)                                                                          { Expr.list_lit     (Loc.mk $loc)  l                                            }
  | x=var                                                                                     { Expr.var          (Loc.mk $loc)  x                                            }
  | "(" e=expr ")"                                                                            {                                  e                                            }

%inline bop:
  | "+"                                                                                       { Bop.add           (Loc.mk $loc)                                               }
  | "-"                                                                                       { Bop.sub           (Loc.mk $loc)                                               }
  | "*"                                                                                       { Bop.mul           (Loc.mk $loc)                                               }
  | "/"                                                                                       { Bop.div           (Loc.mk $loc)                                               }
  | "mod"                                                                                     { Bop.mod_          (Loc.mk $loc)                                               }
  | "&&"                                                                                      { Bop.and_          (Loc.mk $loc)                                               }
  | "||"                                                                                      { Bop.or_           (Loc.mk $loc)                                               }
  | "::"                                                                                      { Bop.cons          (Loc.mk $loc)                                               }
  | "="                                                                                       { Bop.eq            (Loc.mk $loc)                                               }
  | "<>"                                                                                      { Bop.neq           (Loc.mk $loc)                                               }
  | "<"                                                                                       { Bop.lt            (Loc.mk $loc)                                               }
  | "<="                                                                                      { Bop.lte           (Loc.mk $loc)                                               }
  | ">"                                                                                       { Bop.gt            (Loc.mk $loc)                                               }
  | ">="                                                                                      { Bop.gte           (Loc.mk $loc)                                               }
