(* File lexer.mll *)
{
open Parser
exception Eof
exception Error of string

(* Function to create an error message that includes the position in the input *)
let error_msg lexbuf msg =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.sprintf "Error at line %d, position %d: %s" pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) msg
}

rule token = parse
    [' ' '\t'] { token lexbuf } (* skip blanks and tabs *)
  | ['\n' ]    { EOL }
  | ';'        { SEMICOLON }
  | ':'        { COLON }
  | "=="       { EQUAL }
  | '='        { ASSIGN }
  | '.'        { DOT }
  | '<'        { LESS }
  | "if"       { IF }
  | "else"     { ELSE }
  | "then"     { THEN }
  | "var"      { VAR }
  | "malloc"   { MALLOC }
  | "proc"     { PROC }
  | "while"    { WHILE }
  | "skip"     { SKIP }
  | '-'        { MINUS }
  | '+'        { PLUS }
  | '('        { LPAREN }
  | ')'        { RPAREN }
  | '{'        { LBRACE }
  | '}'        { RBRACE }
  | "true"     { TRUE }
  | "false"    { FALSE }
  | "|||"      { PARALLEL }
  | "atom"     { ATOM }
  | "null"     { NULL }
  | ['A'-'Z'](['a'-'z'] | ['A'-'Z'] | ['0'-'9'])* as var
    { VARIABLE var }
  | ['a'-'z'](['a'-'z'] | ['A'-'Z'] | ['0'-'9'])* as field
    { FIELD field }
  | ['0'-'9']+ as num
    { NUMBER (int_of_string num) }
  | eof        { raise Eof }
  | _ as char  { raise (Error (error_msg lexbuf (Printf.sprintf "Unexpected character: '%c'" char))) }