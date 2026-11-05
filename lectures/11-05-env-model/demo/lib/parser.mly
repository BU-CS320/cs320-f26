%{
open Syntax
%}

%token EOF
%token FUN
%token ARROW
%token LPAREN
%token RPAREN
%token LET
%token EQ
%token IN
%token <string> VAR
%token <int> NUM
%token IF
%token THEN
%token ELSE
%token PLUS

%left EQ
%left PLUS

%start <Syntax.expr> prog

%%

prog:
  | e = expr EOF { e }

expr:
  | LET; x=VAR; EQ; e1=expr; IN; e2=expr { Let(x, e1, e2) }
  | IF; e1=expr; THEN; e2=expr; ELSE; e3=expr { If (e1, e2, e3) }
  | FUN; x=VAR; ARROW; e=expr { Fun(x, e) }
  | e = expr2 { e }

expr2:
  | e1 = expr2; PLUS; e2 = expr2 { Add (e1, e2) }
  | e1 = expr2; EQ; e2 = expr2 { Eq (e1, e2) }
  | e = expr3; es = expr3*
    { List.fold_left (fun e1 e2 -> App (e1, e2)) e es }

expr3:
  | x = VAR { Var x }
  | n = NUM { Num n }
  | LPAREN; e=expr; RPAREN { e }
