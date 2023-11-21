/* File parser.mly */

%{ (* header *)
  
(* open Declarations *)

%} /* declarations */

/* lexer tokens */
%token EOL SEMICOLON COLON ASSIGN MINUS
%token VAR NULL PROC DOT
%token TRUE FALSE LESS EQUAL
%token VARIABLE
%token FIELD
%token NUMBER
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

main:
    cmds EOL { () }

cmds :
    cmd SEMICOLON l = cmds   { () }
    | c = cmd                  { () }

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
    | LBRACE cmd PARALLELPIPE cmd RBRACE { () }
    | ATOM LPAREN cmd RPAREN { () }

expr: FIELD { () }
    | NUMBER { () }
    | expr MINUS expr { () }
    | NULL { () }
    | VARIABLE { () }
    | expr DOT expr { () }
    | PROC VARIABLE COLON command { () }

bool: TRUE { () }
    | FALSE { () }
    | expr EQUAL expr { () }
    | expr LESS expr { () }
%%