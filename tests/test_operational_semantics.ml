(* File: test_operational_semantics.ml *)

open Lexer
open Parser
open OUnit2
open Semantic_domain
open Ast
open Operational_semantics

let test_operational_behavior input expected_state =
  reset_location_counter ();
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

(* Define expected states for tests *)
let expected_state_for_var_assignment = {
  stack = [Decl [("X", ObjectLoc (Object 0))]];
  heap = let h = Hashtbl.create 100 in
         Hashtbl.add h (ObjectLoc (Object 0), "val") (Val (Int(5)));
         h
}

(* Define expected states for tests *)
let expected_state_for_minus = {
  stack = [Decl [("X", ObjectLoc (Object 0))]];
  heap = let h = Hashtbl.create 100 in
         Hashtbl.add h (ObjectLoc (Object 0), "val") (Val (Int(3)));
         h
}

(* Define expected states for tests *)
let expected_state_for_multiple_commands = {
  stack = [Decl [("P", ObjectLoc (Object 2))]; Decl [("H", ObjectLoc (Object 1))]; Decl [("R", ObjectLoc (Object 0))]];
  heap = let h = Hashtbl.create 100 in
    Hashtbl.add h (ObjectLoc (Object 0), "val") (Val Null);
    Hashtbl.add h (ObjectLoc (Object 1), "val") (Val (Int(1)));
    Hashtbl.add h (ObjectLoc (Object 2), "val") (Val (Int(0)));
    h
}

let expected_state_for_procedure_declaration = {
  stack = [
    Decl [("H", ObjectLoc (Object 3))];
    Decl [("P", ObjectLoc (Object 2))];
    Decl [("H", ObjectLoc (Object 1))];
    Decl [("R", ObjectLoc (Object 0))]
  ];
  heap = let h = Hashtbl.create 100 in
    (* Add entry for R at Object 0 *)
    Hashtbl.add h (ObjectLoc (Object 0), "val") (Val Null);

    (* Add entry for H at Object 1 with value 1 *)
    Hashtbl.add h (ObjectLoc (Object 1), "val") (Val (Int 1));

    (* Add closure for P at Object 2 *)
    let closure_body = Assign ("R", Minus (Variable "Y", Variable "H")) in
    let closure_env = [
      Decl [("P", ObjectLoc (Object 2))];
      Decl [("H", ObjectLoc (Object 1))];
      Decl [("R", ObjectLoc (Object 0))]
    ] in
    Hashtbl.add h (ObjectLoc (Object 2), "val") (Val (Clo (Closure ("Y", closure_body, closure_env))));

    (* Add entry for H at Object 3 with value 2 *)
    Hashtbl.add h (ObjectLoc (Object 3), "val") (Val (Int 2));

    h
}

(* Define test cases *)
let test_cases = [
  "test_var_declaration" >:: (fun _ ->
    test_operational_behavior "var X\n" expected_state_for_var_declaration
  );
  "test_var_assignment" >:: (fun _ ->
    test_operational_behavior "var X; X = 5\n" expected_state_for_var_assignment
  );
  "test_minus" >:: (fun _ ->
    test_operational_behavior "var X; X = 10 - 7\n" expected_state_for_minus
  );
  "test_multiple_commands" >:: (fun _ ->
    test_operational_behavior "var R; var H; H=1; var P; P = 5 - 5\n" expected_state_for_multiple_commands
  );
  "test_procedure_declaration" >:: (fun _ ->
    test_operational_behavior "var R; var H; H=1; var P; P = proc Y: R = Y - H; var H; H = 2\n" expected_state_for_procedure_declaration
  );
]

(* Run tests *)
let () =
  Printf.printf "Starting operational semantics tests.\n";
  run_test_tt_main ("Test suite for operational semantics" >::: test_cases);
  Printf.printf "All tests completed.\n";
