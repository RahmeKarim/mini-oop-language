(* File: test_parser.ml *)

open OUnit2
open Ast
open Parser
open Pretty_printer
open Lexer

(* Example AST for the input "var H" *)
let expected_output_for_var : Ast.cmd list = [
  Var "H"
]

(* Example AST for the input "var H; H = 5" *)
let expected_output_for_var_assignment : Ast.cmd list = [
  Var "H";
  Assign ("H", Number 5)
]

(* Example AST for the input "var R; var H; H=1; var P; P = proc Y: R = Y - H; var H; H = 2; P(4);" *)
let expected_output_for_example1 : Ast.cmd list = [
  Var "R";
  Var "H"; 
  Assign ("H", Number 1);
  Var "P";
  Assign (
    "P",
    Proc(
      "Y",
      Assign (
        "R",
        Minus(Variable "Y", Variable "H")
      )
    )
  );
  Var "H";
  Assign ("H", Number 2);
  Call (Variable "P", Number 4)
]

(* Example AST for the input "var P; P = proc Y:if Y < 1 then P = 1 else P(Y - 1); P(1);" *)
let expected_output_for_example2 : Ast.cmd list = [
  Var "P";
  Assign (
    "P",
    Proc (
      "Y",
      IfThenElse(
        Less(
          Variable "Y",
          Number 1
        ),
        Assign (
          "P",
          Number 1
        ),
        Call(
          Variable "P",
          Minus(
            Variable "Y",
            Number 1
          )
        )
      );
    );
  );
  Call(
    Variable "P",
    Number 1
  );
]

(* Example AST for the input "var X; malloc(X); X.c = 0; X.f = proc Y: if Y < 1 then X.r = X.c else X.f(Y - 1); X.f(2)" *)
let expected_output_for_example3 : Ast.cmd list = [
  Var "X";
  Malloc "X";
  FieldAssignExpression(
    Variable "X",
    Field "c",
    Number 0
  );
  FieldAssignExpression(
    Variable "X",
    Field "f",
    Proc (
      "Y",
      IfThenElse(
        Less(
          Variable "Y",
          Number 1
        ),
        FieldAssignExpression(
          Variable "X",
          Field "r",
          FieldValue(
            Variable "X",
            Field "c"
          )
        ),
        Call(
          FieldValue(
            Variable "X",
            Field "f"
          ),
          Minus(
            Variable "Y",
            Number 1
          )
        )
      )
    )
  );
  Call(
    FieldValue(
      Variable "X",
      Field "f"
    ),
    Number 2
  );
]

let test_parser input expected_output =
  let lexbuf = Lexing.from_string input in (* Convert input string to lexbuf *)
  let parsed_ast = Parser.main Lexer.token lexbuf in
    if parsed_ast = expected_output then () (* Test passed *)
    else
      begin
        Printf.printf "Expected:\n%s\n" (string_of_ast expected_output);
        Printf.printf "Actual:\n%s\n" (string_of_ast parsed_ast);
        assert_failure "Parsed output does not match expected output"
      end


(* Test cases *)
let test_cases = [
  "test_var" >:: (fun _ ->
    test_parser "var H\n" expected_output_for_var
  );
  "test_var_assignment" >:: (fun _ ->
    test_parser "var H; H = 5\n" expected_output_for_var_assignment
  );
  "example_1" >:: (fun _ -> 
    test_parser "var R; var H; H=1; var P; P = proc Y: R = Y - H; var H; H = 2; P(4)\n" expected_output_for_example1
  );
  "example_2" >:: (fun _ -> 
    test_parser "var P; P = proc Y: if Y < 1 then P = 1 else P(Y - 1); P(1)\n" expected_output_for_example2
  );
  "example_3" >:: (fun _ -> 
    test_parser "var X; malloc(X); X.c = 0; X.f = proc Y: if Y < 1 then X.r = X.c else X.f(Y - 1); X.f(2)\n" expected_output_for_example3
  );
]

(* Run tests *)
let () =
  run_test_tt_main ("Test suite for parser" >::: test_cases)
