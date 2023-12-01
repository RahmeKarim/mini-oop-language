/* File parser.mly */

%{
  (* Header *)
  open Parsing
  open Ast

  let report_error msg =
  Printf.eprintf "%s\n" msg
%}

%token EOL SEMICOLON COLON ASSIGN MINUS /* lexer tokens */
%token VAR NULL PROC DOT
%token TRUE FALSE LESS EQUAL
%token <string> VARIABLE
%token <string> FIELD
%token <int> NUMBER
%token LPAREN RPAREN
%token MALLOC SKIP LBRACE RBRACE
%token WHILE IF THEN ELSE PARALLEL ATOM

%right ASSIGN   /* Lowest precedence */
%left  MINUS     /* Medium precedence */
%left  DOT       /* Highest precedence */

%start main
%type <cmds> main

%%

main:
  cs = cmds EOL { cs }
  | error { report_error "Syntax error at end of input"; [] }

cmds :
  c = cmd SEMICOLON l = cmds   { c :: l }
  | c = cmd                    { [c] }
  |                            { [] }

cmd:
  VAR v = VARIABLE                            { Var v }
  | e1 = expr LPAREN e2 = expr RPAREN         { Call(e1, e2) }
  | MALLOC LPAREN v = VARIABLE RPAREN         { Malloc v }
  | v = VARIABLE ASSIGN e = expr              { Assign (v, e) }
  | e1 = expr DOT f = FIELD ASSIGN e2 = expr  { FieldAssignExpression(e1, Field f, e2) }
  | SKIP                                      { Skip }
  | LBRACE cs = cmds RBRACE                   { Block cs }
  | WHILE b = bool c = cmd                    { While (b, c) }
  | IF b = bool THEN c1 = cmd ELSE c2 = cmd        { IfThenElse (b, c1, c2) }
  | LBRACE c1 = cmd PARALLEL c2 = cmd RBRACE  { Parallel (c1, c2) }
  | ATOM LPAREN c = cmd RPAREN                { Atom c }

expr:
  f = FIELD                         { Field f }
  | n = NUMBER                      { Number n }
  | e1 = expr MINUS e2 = expr       { Minus (e1, e2) }
  | NULL                            { Null }
  | v = VARIABLE                    { Variable v }
  | e1 = expr DOT e2 = expr         { FieldValue (e1, e2) }
  | PROC v = VARIABLE COLON c = cmd { Proc (v, c) }

bool:
  TRUE                         { True }
  | FALSE                      { False }
  | e1 = expr EQUAL e2 = expr  { Equal (e1, e2) }
  | e1 = expr LESS e2 = expr   { Less (e1, e2) }

%%