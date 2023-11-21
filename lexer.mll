(* File lexer.mll *)
{
open Parser  (* Type token defined in parser.mli *)
exception Eof 
}

rule token = parse
    [' ' '\t' '\n'] { token lexbuf } (* skip blanks and tabs *)
  | ['A'-'Z'](['a'-'z'] | ['A'-'Z'] | ['0'-'9'])* as var
    { VARIABLE var }
  | ['a'-'z'](['a'-'z'] | ['A'-'Z'] | ['0'-'9'])* as field
    { FIELD field }
  | ['0'-'9']+ as num
    { NUMBER (int_of_string num) }
  | ['\n' ]    { EOL }
  | ';'        { SEMICOLON }
  | ':'        { COLON }
  | "=="       { EQUAL }
  | '='        { ASSIGN }
  | '.'        { DOT }
  | '<'        { LESS }
  | "if"       { IF }
  | "else"     { ELSE }
  | "var"      { VAR }
  | "malloc"   { MALLOC }
  | "proc"     { PROC }
  | "while"    { WHILE }
  | '-'        { MINUS }
  | '('        { LPAREN }
  | ')'        { RPAREN }
  | '{'        { LBRACE }
  | '}'        { RBRACE }
  | "true"     { TRUE }
  | "false"    { FALSE }
  | "|||"      { PARALLEL }
  | "atom"     { ATOM }
  | "null"     { NULL }
  | eof        { raise Eof }
