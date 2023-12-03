(* File: test_operational_semantics.ml *)

open Lexer
open Parser
open OUnit2
open Semantic_domain
open Operational_semantics

let test_operational_behavior input expected_state =
  let lexbuf = Lexing.from_string input in
  let ast_cmds = Parser.main Lexer.token lexbuf in
  let semantic_controls = convert_cmds_to_control ast_cmds in
  let actual_state = eval_prog semantic_controls in
  if actual_state = expected_state then () (* Test passed *)
  else
    begin
      Printf.printf "\n\nExpected state did not match actual state.\n";
      Printf.printf "Actual State: %s\n" (string_of_state actual_state);
      Printf.printf "\nExpected State: %s\n" (string_of_state expected_state);
      assert_failure "Operational behavior does not match expected output"
    end

(* Define expected states for tests *)
let expected_state_for_var_declaration = {
  stack = [Decl [("X", ObjectLoc (Object 0))]];
  heap = let h = Hashtbl.create 100 in
         Hashtbl.add h (ObjectLoc (Object 0), "val") (Val Null);
         h
}

(* Define test cases *)
let test_cases = [
  "test_var_declaration" >:: (fun _ ->
    test_operational_behavior "var X\n" expected_state_for_var_declaration
  );
]

(* Run tests *)
let () =
  Printf.printf "Starting operational semantics tests.\n";
  run_test_tt_main ("Test suite for operational semantics" >::: test_cases);
  Printf.printf "All tests completed.\n";
