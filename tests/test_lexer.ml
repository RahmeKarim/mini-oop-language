open Lexer
open Lexing
open Parser

let string_of_token = function
  | EOL -> "EOL"
  | SEMICOLON -> "SEMICOLON"
  | COLON -> "COLON"
  | ASSIGN -> "ASSIGN"
  | MINUS -> "MINUS"
  | VAR -> "VAR"
  | NULL -> "NULL"
  | PROC -> "PROC"
  | DOT -> "DOT"
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | LESS -> "LESS"
  | EQUAL -> "EQUAL"
  | VARIABLE v -> "VARIABLE(" ^ v ^ ")"
  | FIELD f -> "FIELD(" ^ f ^ ")"
  | NUMBER n -> "NUMBER(" ^ string_of_int n ^ ")"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | MALLOC -> "MALLOC"
  | SKIP -> "SKIP"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | WHILE -> "WHILE"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | PARALLEL -> "PARALLEL"
  | ATOM -> "ATOM"
  | EOF -> "EOF"

let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

  let rec print_tokens lexbuf =
    try
      match Lexer.token lexbuf with
      | EOF -> Printf.printf "EOF\n"
      | token ->
        Printf.printf "%s %s\n" (print_position lexbuf) (string_of_token token);
        print_tokens lexbuf
    with Lexer.Eof ->
      Printf.printf "End of File reached.\n"
  
let () =
  let input = "var H; H = 5" in
  let lexbuf = from_string input in
  print_tokens lexbuf
