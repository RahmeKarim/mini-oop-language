open OUnit2
open Parser
open Lexer

(* Define expected outputs *)
let expected_output_for_varr_varh = () (* Define the expected AST or output for this input *)
let expected_output_for_varp = () (* Define the expected AST or output for this input *)
let expected_output_for_varx_malloc = () (* Define the expected AST or output for this input *)

(* A helper function to run the parser on a given input string *)
let parse_string str =
  let lexbuf = Lexing.from_string str in
  try
    Some (Parser.main Lexer.token lexbuf)
  with
  | _ -> None  (* Or handle specific exceptions *)

(* Define test cases *)
let test_cases = [
  "test_var" >:: (fun _ ->
    assert_equal (Some expected_output_for_varr_varh) (parse_string "var H; H = 5")
  );
]

(* Run tests *)
let () =
  run_test_tt_main ("Test suite for parser" >::: test_cases)
