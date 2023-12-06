(* File: test_operational_semantics.ml *)

open Lexer
open Parser
open OUnit2
open Semantic_domain
open Ast
open Operational_semantics

let test_operational_behavior input expected_state =
  let lexbuf = Lexing.from_string input in
  let ast_cmds = Parser.main Lexer.token lexbuf in
  let actual_state = eval_prog ast_cmds in
  if actual_state = expected_state then () (* Test passed *)
  else
    begin
      Printf.printf "\n\nExpected state did not match actual state.\n";
      Printf.printf "Actual State: %s\n" (string_of_state actual_state);
      Printf.printf "\nExpected State: %s\n" (string_of_state expected_state);
      assert_failure "Operational behavior does not match expected output"
    end

(* Expected states for tests *)
let expected_state_for_multiple_commands = {
  stack = [Decl [("P", ObjectLoc (Object 2))]; Decl [("H", ObjectLoc (Object 1))]; Decl [("R", ObjectLoc (Object 0))]];
  heap = let h = Hashtbl.create 100 in
    Hashtbl.add h (ObjectLoc (Object 0), "val") (Val Null);
    Hashtbl.add h (ObjectLoc (Object 1), "val") (Val (Int(1)));
    Hashtbl.add h (ObjectLoc (Object 2), "val") (Val (Int(0)));
    h
}
let expected_state_for_var_declaration = {
  stack = [];
  heap = let h = Hashtbl.create 100 in
         Hashtbl.add h (ObjectLoc (Object 0), "val") (Val Null);
         h
}

let expected_state_for_var_assignment = {
  stack = [];
  heap = let h = Hashtbl.create 100 in
         Hashtbl.add h (ObjectLoc (Object 0), "val") (Val (Int(5)));
         h
}

let expected_state_for_minus = {
  stack = [];
  heap = let h = Hashtbl.create 100 in
         Hashtbl.add h (ObjectLoc (Object 0), "val") (Val (Int(3)));
         h
}

let expected_state_for_multiple_commands = {
  stack = [];
  heap = let h = Hashtbl.create 100 in
    Hashtbl.add h (ObjectLoc (Object 0), "val") (Val Null);
    Hashtbl.add h (ObjectLoc (Object 1), "val") (Val (Int(1)));
    Hashtbl.add h (ObjectLoc (Object 2), "val") (Val (Int(0)));
    h
}

let expected_state_for_procedure_declaration = {
  stack = [];
  heap = let h = Hashtbl.create 100 in
    Hashtbl.add h (ObjectLoc (Object 0), "val") (Val Null);

    Hashtbl.add h (ObjectLoc (Object 1), "val") (Val (Int 1));

    let closure_body = Assign ("R", Minus (Variable "Y", Variable "H")) in
    let closure_env = [
      Decl [("P", ObjectLoc (Object 2))];
      Decl [("H", ObjectLoc (Object 1))];
      Decl [("R", ObjectLoc (Object 0))]
    ] in
    Hashtbl.add h (ObjectLoc (Object 2), "val") (Val (Clo (Closure ("Y", closure_body, closure_env))));

    Hashtbl.add h (ObjectLoc (Object 3), "val") (Val (Int 2));

    h
}

let expected_state_for_malloc = {
  stack = [];
  heap = let h = Hashtbl.create 100 in
    Hashtbl.add h (ObjectLoc (Object 0), "val") (Val (Loc (ObjectLoc (Object 1))));
    h
}

let expected_state_for_field_assignment = {
  stack = [];
  heap = let h = Hashtbl.create 100 in
    Hashtbl.add h (ObjectLoc (Object 0), "val") (Val (Loc (ObjectLoc (Object 1))));
    Hashtbl.add h (ObjectLoc (Object 1), "f") (Val (Int (3)));
    h
}

let expected_state_for_malloc_with_extra_fields = {
  stack = [];
  heap = let h = Hashtbl.create 100 in
    Hashtbl.add h (ObjectLoc (Object 0), "val") (Val (Loc (ObjectLoc (Object 2))));
    Hashtbl.add h (ObjectLoc (Object 1), "val") (Val (Loc (ObjectLoc (Object 3))));
    Hashtbl.add h (ObjectLoc (Object 2), "f") (Val (Int(3)));
    Hashtbl.add h (ObjectLoc (Object 3), "f") (Val (Int(1)));
    h
}

let expected_state_for_if_then_else_true = {
  stack = [];
  heap = let h = Hashtbl.create 100 in
    Hashtbl.add h (ObjectLoc (Object 0), "val") (Val (Int (1)));
    h
}

let expected_state_for_if_then_else_false = {
  stack = [];
  heap = let h = Hashtbl.create 100 in
    Hashtbl.add h (ObjectLoc (Object 0), "val") (Val (Int (2)));
    h
}

let expected_state_for_while_true = {
  stack = [];
  heap = let h = Hashtbl.create 100 in
    Hashtbl.add h (ObjectLoc (Object 0), "val") (Val (Int (5)));
    h
}

let expected_state_for_while_false = {
  stack = [];
  heap = let h = Hashtbl.create 100 in
    Hashtbl.add h (ObjectLoc (Object 0), "val") (Val (Int (0)));
    h
}

let expected_state_for_skip = {
  stack = [];
  heap = let h = Hashtbl.create 100 in
    Hashtbl.add h (ObjectLoc (Object 0), "val") (Val (Int (2)));
    h
}

let expected_state_for_procedure_call = {
  stack = [];
  heap = let h = Hashtbl.create 100 in
    Hashtbl.add h (ObjectLoc (Object 0), "val") (Val (Int (18)));

    let closure_body = Assign ("X", Minus (Variable "Y", Number 3)) in
    let closure_env = [
      Decl [("P", ObjectLoc (Object 1))];
      Decl [("X", ObjectLoc (Object 0))]
    ] in
    Hashtbl.add h (ObjectLoc (Object 1), "val") (Val (Clo (Closure ("Y", closure_body, closure_env))));
    Hashtbl.add h (ObjectLoc (Object 2), "val") (Val (Int (21)));

    h
}

let expected_state_for_simple_block = {
  stack = [];
  heap = let h = Hashtbl.create 100 in
         Hashtbl.add h (ObjectLoc (Object 0), "val") (Val (Int(5)));
         h
}

let expected_state_for_atom = {
  stack = [];
  heap = let h = Hashtbl.create 100 in
         Hashtbl.add h (ObjectLoc (Object 0), "val") (Val (Int(0)));
         h
}

let expected_state_for_block_procedure_call = {
  stack = [];
  heap = let h = Hashtbl.create 100 in
    Hashtbl.add h (ObjectLoc (Object 0), "val") (Val (Int (8)));

    let closure_body = Block([
      Assign ("Y", Plus (Variable "Y", Number 3));
      Assign ("X", Minus (Variable "Y", Number 5))
    ]) in
    let closure_env = [
      Decl [("P", ObjectLoc (Object 1))];
      Decl [("X", ObjectLoc (Object 0))]
    ] in
    Hashtbl.add h (ObjectLoc (Object 1), "val") (Val (Clo (Closure ("Y", closure_body, closure_env))));
    Hashtbl.add h (ObjectLoc (Object 2), "val") (Val (Int (13)));

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
  "test_malloc" >:: (fun _ ->
    test_operational_behavior "var X; malloc(X)\n" expected_state_for_malloc
  );
  "test_field_assigment" >:: (fun _ ->
    test_operational_behavior "var X; malloc(X); X.f = 3;\n" expected_state_for_field_assignment
  );
  "test_double_field_assigment" >:: (fun _ ->
    test_operational_behavior "var X; var R; malloc(X); malloc(R); X.f = 3; R.f = X.f - 2;\n" expected_state_for_malloc_with_extra_fields
  );
  "test_if_then_else_true" >:: (fun _ ->
    test_operational_behavior "var X; if true then X = 1 else X = 2\n" expected_state_for_if_then_else_true
  );
  "test_if_then_else_false" >:: (fun _ ->
    test_operational_behavior "var X; if false then X = 1 else X = 2\n" expected_state_for_if_then_else_false
  );
  "test_while_true" >:: (fun _ ->
    test_operational_behavior "var X; X = 0; while X < 5 X = X + 1\n" expected_state_for_while_true
  );
  "test_while_false" >:: (fun _ ->
    test_operational_behavior "var X; X = 0; while false X = X + 1\n" expected_state_for_while_false
  );
  "test_skip" >:: (fun _ ->
    test_operational_behavior "var X; X = 3; skip; X = X - 1\n" expected_state_for_skip
  );
  "test_procedure_call" >:: (fun _ ->
    test_operational_behavior "var X; var P; P = proc Y: X = Y - 3; P(21);\n" expected_state_for_procedure_call
  );
  "test_simple_block" >:: (fun _ ->
    test_operational_behavior "{var X; X = 5;}\n" expected_state_for_simple_block
  );
  "test_block_procedure" >:: (fun _ ->
    test_operational_behavior "var X; var P; P = proc Y: { Y = Y + 3; X = Y - 5;}; P(10);\n" expected_state_for_block_procedure_call
  );
]

(* Run tests *)
let () =
  Printf.printf "Starting operational semantics tests.\n";
  run_test_tt_main ("Test suite for operational semantics" >::: test_cases);
  Printf.printf "All tests completed.\n";
