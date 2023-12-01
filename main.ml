(* File main.ml *)
open Ast
open Parser
open Pretty_printer
open Parsing

(* Function to print the position of an error *)
let print_error_position lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.eprintf "Error at line %d, position %d\n" pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);;

try
  let lexbuf = Lexing.from_channel stdin in
  while true do
    try
      (* Parse input to produce AST *)
      let parsed_ast = Parser.main Lexer.token lexbuf in

      (* Print the parsed AST *)
      Printf.printf "Parsed AST:\n%s\n" (string_of_ast parsed_ast);
      print_newline ();

    with
    | Parser.Error ->
      print_error_position lexbuf;
      Printf.eprintf "Syntax error: unexpected token '%s'\n" (Lexing.lexeme lexbuf);
      print_newline ();
      exit 1
    | Failure msg ->
      print_error_position lexbuf;
      Printf.eprintf "Failure: %s\n" msg;
      print_newline ();
      exit 1
  done
with Lexer.Eof ->
  Printf.eprintf "End of file reached.\n";
  exit 0
;;
