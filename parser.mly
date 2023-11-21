/* File parser.mly */

%{
  (* Header *)
  open Parsing
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
%token WHILE IF ELSE PARALLEL ATOM

%right ASSIGN   /* Lowest precedence */
%left MINUS     /* Medium precedence */
%left DOT       /* Highest precedence */

%start main
%type <unit> main
%type <unit> cmd
%type <unit> cmds
%type <unit> expr
%type <unit> bool

%%

main:
  cmds EOL { () }
  | error { report_error "Syntax error at end of input"; () }

cmds :
  cmd SEMICOLON l = cmds   { () }
  | c = cmd                { () }
  | error { report_error "Syntax error in command sequence"; clear_parser(); () }

cmd:
  VAR VARIABLE { () }
  | expr LPAREN expr RPAREN { () }
  | MALLOC LPAREN VARIABLE RPAREN { () }
  | VARIABLE ASSIGN expr { () }
  | expr DOT FIELD ASSIGN expr { () }
  | SKIP { () }
  | LBRACE cmds RBRACE { () }
  | WHILE bool cmd { () }
  | IF bool cmd ELSE cmd { () }
  | LBRACE cmd PARALLEL cmd RBRACE { () }
  | ATOM LPAREN cmd RPAREN { () }
  | error { report_error "Syntax error in command"; clear_parser(); () }

expr:
  FIELD { () }
  | NUMBER { () }
  | expr MINUS expr { () }
  | NULL { () }
  | VARIABLE { () }
  | expr DOT expr { () }
  | PROC VARIABLE COLON cmd { () }
  | error { report_error "Syntax error in expression"; clear_parser(); () }

bool:
  TRUE { () }
  | FALSE { () }
  | expr EQUAL expr { () }
  | expr LESS expr { () }
  | error { report_error "Syntax error in boolean expression"; clear_parser(); () }

%%