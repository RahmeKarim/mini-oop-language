(* File: test_static_analyzer.ml *)

open OUnit2
open Ast
open Static_analyzer

let test_static_analysis input expected_scope =
  Printf.printf "\nStarting test for: %s\n" input;
  let lexbuf = Lexing.from_string input in
  let ast = Parser.main Lexer.token lexbuf in
  let actual_scope = run_analysis ast ~print_scope:false in
    if actual_scope.e = expected_scope.e then () (* Test passed *)
    else
      begin
        Printf.printf "Expected error flag: %b\n" expected_scope.e;
        Printf.printf "Actual error flag: %b\n" actual_scope.e;
        assert_failure "Static analysis error flag does not match expected output"
      end

(* Example expected scopes for the inputs *)
let expected_scope_for_var = { v = ["H"]; u = ["H"]; e = false }
let expected_scope_for_var_assignment = { v = ["H"]; u = ["H"]; e = false }
let expected_scope_for_example1 = { v = ["R"; "H"; "P"; "H"]; u = ["R"; "H"; "P"; "H"]; e = false }
let expected_scope_for_example2 = { v = ["P"]; u = ["P"];e = false }
let expected_scope_for_example3 = { v = ["X"]; u = ["X"];e = false }

(* Example expected scopes with errors for the inputs *)
let expected_scope_for_undeclared_use = { v = []; u = [];e = true }
let expected_scope_for_undeclared_in_proc = { v = ["P"]; u = ["P"]; e = true }
let expected_scope_for_undeclared_in_while = { v = ["X"]; u = ["X"]; e = false }

(* Test cases *)
let test_cases = [
  "test_var" >:: (fun _ ->
    test_static_analysis "var H\n" expected_scope_for_var
    );

  "test_var_assignment" >:: (fun _ ->
    test_static_analysis "var H; H = 5\n" expected_scope_for_var_assignment
    );

  "example_1" >:: (fun _ -> 
    test_static_analysis "var R; var H; H=1; var P; P = proc Y: R = Y - H; var H; H = 2; P(4)\n" expected_scope_for_example1
  );

  "example_2" >:: (fun _ -> 
    test_static_analysis "var P; P = proc Y: if Y < 1 then P = 1 else P(Y - 1); P(1)\n" expected_scope_for_example2
  );

  "example_3" >:: (fun _ -> 
    test_static_analysis "var X; malloc(X); X.c = 0; X.f = proc Y: if Y < 1 then X.r = X.c else X.f(Y - 1); X.f(2)\n" expected_scope_for_example3
  );

  "undeclared_variable_use" >:: (fun _ ->
    test_static_analysis "H = 5\n" expected_scope_for_undeclared_use
  );

  "undeclared_variable_in_proc" >:: (fun _ ->
    test_static_analysis "var P; P = proc Y: R = Y - 1; P(4)\n" expected_scope_for_undeclared_in_proc
  );

  "undeclared_variable_in_while" >:: (fun _ ->
    test_static_analysis "var X; while true X = X - 1\n" expected_scope_for_undeclared_in_while
  );
]

(* Run tests *)
let () =
  run_test_tt_main ("Test suite for static analyzer" >::: test_cases)
