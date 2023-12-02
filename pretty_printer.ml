(* File: pretty_printer.ml *)
open Ast
open Format

(* Indentation helper function *)
let indent fmt depth =
  for _ = 1 to depth do
    fprintf fmt "  " (* Two spaces for each level of depth *)
  done

(* Recursive functions to print expressions and commands with indentation *)
let rec print_expr fmt depth expr =
  indent fmt depth;
  match expr with
  | Field f -> fprintf fmt "Field %s\n" f
  | Number n -> fprintf fmt "Number %d\n" n
  | Minus (e1, e2) ->
      fprintf fmt "Minus\n";
      print_expr fmt (depth + 1) e1;
      print_expr fmt (depth + 1) e2
  | Null -> fprintf fmt "Null\n"
  | Variable v -> fprintf fmt "Variable %s\n" v
  | Proc (v, c) ->
      fprintf fmt "Proc (%s)\n" v;
      print_cmd fmt (depth + 1) c
  | FieldValue (e1, e2) ->
      fprintf fmt "FieldValue\n";
      print_expr fmt (depth + 1) e1;
      print_expr fmt (depth + 1) e2

and print_cmd fmt depth cmd =
  indent fmt depth;
  match cmd with
  | Var v -> fprintf fmt "Var %s\n" v
  | Assign (v, e) ->
      fprintf fmt "Assign (%s)\n" v;
      print_expr fmt (depth + 1) e
  | Malloc v -> fprintf fmt "Malloc %s\n" v
  | Skip -> fprintf fmt "Skip\n"
  | Block cmds ->
      fprintf fmt "Block\n";
      List.iter (print_cmd fmt (depth + 1)) cmds
  | While (b, c) ->
      fprintf fmt "While\n";
      print_bool_expr fmt (depth + 1) b;
      print_cmd fmt (depth + 1) c
  | IfThenElse (b, c1, c2) ->
      fprintf fmt "IfThenElse\n";
      print_bool_expr fmt (depth + 1) b;
      print_cmd fmt (depth + 1) c1;
      print_cmd fmt (depth + 1) c2
  | FieldAssignExpression (e1, e2, e3) ->
      fprintf fmt "FieldAssignExpression\n";
      print_expr fmt (depth + 1) e1;
      print_expr fmt (depth + 1) e2;
      print_expr fmt (depth + 1) e3
  | Parallel (c1, c2) ->
      fprintf fmt "Parallel\n";
      print_cmd fmt (depth + 1) c1;
      print_cmd fmt (depth + 1) c2
  | Atom c ->
      fprintf fmt "Atom\n";
      print_cmd fmt (depth + 1) c
  | Call (e1, e2) ->
      fprintf fmt "Call\n";
      print_expr fmt (depth + 1) e1;
      print_expr fmt (depth + 1) e2

and print_bool_expr fmt depth bexpr =
  indent fmt depth;
  match bexpr with
  | True -> fprintf fmt "True\n"
  | False -> fprintf fmt "False\n"
  | Equal (e1, e2) ->
      fprintf fmt "Equal\n";
      print_expr fmt (depth + 1) e1;
      print_expr fmt (depth + 1) e2
  | Less (e1, e2) ->
      fprintf fmt "Less\n";
      print_expr fmt (depth + 1) e1;
      print_expr fmt (depth + 1) e2

and print_cmds fmt cmds =
  List.iter (print_cmd fmt 0) cmds

(* Function to convert AST to string *)
let string_of_ast ast =
  let buffer = Buffer.create 100 in
  let fmt = formatter_of_buffer buffer in
  print_cmds fmt ast;
  pp_print_flush fmt ();
  Buffer.contents buffer