(* File main.ml *)
open Ast
open Parser
open Pretty_printer
open Static_analyzer
open Operational_semantics

(* Function to print the position of an error *)
let print_error_position lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.eprintf "Error at line %d, position %d\n" pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);;

try
  let lexbuf = Lexing.from_channel stdin in
  while true do
    try
      (* Parse input to produce AST *)
      let ast = Parser.main Lexer.token lexbuf in
      begin
        Printf.printf "\nParsed AST:\n%s\n" (string_of_ast ast);
        print_newline ();
    
        let final_scope = run_analysis ast ~print_scope:true in
        begin
          report_unused_variables final_scope;
          print_newline ();
        end;
    
        if final_scope.e then begin
          Printf.printf "Error: Use of undeclared variable\n";
          print_newline ();
        end
        else begin
          if final_scope.e then begin
            Printf.printf "Error: Use of undeclared variable\n";
            print_newline ();
          end
          else begin
            let _ = eval_prog ast in
            print_newline ();
          end;
        end;
      end

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
