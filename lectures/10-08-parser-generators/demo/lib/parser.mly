// %{
//     open Utils
// %}

// %token LET
// %token EQ
// %token IN
// %token EOF
// %token PLUS
// %token MINUS
// %token STAR
// %token SLASH
// %token LPAREN
// %token RPAREN
// %token<string> IDENT
// %token<int> INT

// %start <Utils.prog> prog

// %left PLUS, MINUS
// %left STAR, SLASH

// %%

// prog:
//   | e = expr; EOF { e }

// expr:
//   | LET; x = var; EQ; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }
//   | e = expr1 { e }

// %inline bop:
//   | PLUS { Add }
//   | MINUS { Sub }
//   | STAR { Mul }
//   | SLASH { Div }

// expr1:
//   | e1 = expr1; op = bop; e2 = expr1 { Bop (op, e1, e2) }
//   | n = num { Num n }
//   | x = var { Var x }
//   | LPAREN; e = expr; RPAREN { e }

// num:
//   | n = INT { n }

// var:
//   | x = IDENT { x }


// %{
//   open Utils
// %}

// %token PLUS MINUS STAR SLASH LET EQ IN
// %token EOF
// %token LPAREN RPAREN
// %token<string> IDENT
// %token<int> INT

// %start <Utils.prog> prog

// %left PLUS, MINUS
// %left STAR, SLASH

// %%

// prog:
//   | e = expr; EOF   { e }

// expr:
//   | e1 = expr; PLUS; e2 = expr        { Add(e1, e2) }
//   | e1 = expr; MINUS; e2 = expr       { Sub(e1, e2) }
//   | e1 = expr; STAR; e2 = expr        { Mul(e1, e2) }
//   | e1 = expr; SLASH; e2 = expr       { Div(e1, e2) }
//   | n = INT                           { Num(n) }
//   | x = IDENT                         { Var(x) }           
//   | LPAREN; e = expr; RPAREN          { e }

%{
  open Utils
%}

%token PLUS MINUS STAR SLASH LPAREN RPAREN EOF LET EQ IN
%token<string> IDENT
%token<int> INT

%start <Utils.expr> prog

%left PLUS, MINUS
%left STAR, SLASH


%%

prog:
  | e = expr; EOF     { e }


expr1:
  | e1 = expr1; PLUS; e2 = expr1      { Add(e1, e2) }
  | e1 = expr1; MINUS; e2 = expr1     { Sub(e1, e2) }
  | e1 = expr1; STAR; e2 = expr1      { Mul(e1, e2) }
  | e1 = expr1; SLASH; e2 = expr1     { Div(e1, e2) }
  | LPAREN; e = expr; RPAREN        { e }
  | n = INT                         { Num(n) }
  | x = IDENT                       { Var(x) }


expr:
  | LET; x = IDENT; EQ; e1 = expr; IN; e2 = expr    { Let(x, e1, e2) }
  | e = expr1     { e }

(* let x = 2 in 2 + x *)
(* let x = 2 in (2 + x) *)
(* (let x = 2 in x) + x *)