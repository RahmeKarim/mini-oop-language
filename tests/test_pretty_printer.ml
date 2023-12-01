open OUnit2
open Ast
open Pretty_printer

(* Define expected outputs *)
let expected_output_for_var = 
  "Var H\nAssign (H)\n  Number 5\n"

(* Test function directly working with AST *)
let test_pretty_print input_ast expected_output =
  let ast_string = string_of_ast input_ast in
  if ast_string <> expected_output then begin
    Printf.printf "Expected:\n%s\n" expected_output;
    Printf.printf "Actual:\n%s\n" ast_string;
    assert_failure "Pretty printer output does not match expected output"
  end

(* Test cases *)
let test_cases = [
  "test_var" >:: (fun _ ->
    let ast_test = [Var "H"; Assign ("H", Number 5)] in
    test_pretty_print ast_test expected_output_for_var
  );
]

(* Run tests *)
let () =
  run_test_tt_main ("Test suite for pretty printer" >::: test_cases)
