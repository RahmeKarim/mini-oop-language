(* File: static_analyzer.ml *)

open Ast
open Parser
open Pretty_printer
open Format

type scope_info = {
  v: string list;  (* Declared variables *)
  u: string list;  (* Used variables *)
  e: bool;         (* Error flag for undeclared variable usage *)
}

let rec analyze_expr expr scope =
  match expr with
  | Field _ | Number _ | Null ->
    { scope with e = false }  (* Constants do not generate undeclared variable errors *)
  | Minus (e1, e2) | FieldValue (e1, e2) ->
    let scope1 = analyze_expr e1 scope in
    let scope2 = analyze_expr e2 { scope with v = scope1.v } in
    { scope with e = scope1.e || scope2.e }  (* Propagate error flag *)
  | Variable x ->
    { scope with u = x :: scope.u; e = not (List.mem x scope.v) }  (* Set error flag if variable not declared *)
  | Proc (y, c) ->
    (* Analyze the body of the procedure with 'y' added to the scope *)
    let proc_scope = analyze_cmd c { scope with v = y :: scope.v } in
    { scope with e = proc_scope.e }

and analyze_bool_expr bool_expr scope =
  match bool_expr with
  | True | False ->
    { scope with e = false }  (* Boolean literals do not generate undeclared variable errors *)
  | Equal (e1, e2) | Less (e1, e2) ->
    let scope1 = analyze_expr e1 scope in
    let scope2 = analyze_expr e2 { scope with v = scope1.v } in
    { scope with e = scope1.e || scope2.e }  (* Propagate error flag *)

and analyze_cmd cmd scope =
  match cmd with
  | Var x -> { scope with v = x :: scope.v }
  
  | Assign (x, e) ->
    let expr_scope = analyze_expr e scope in
    { expr_scope with u = x :: expr_scope.u; e = expr_scope.e || not (List.mem x scope.v) }  
  
  | Skip ->  (* Skip does not affect the scope *)
    scope
  
  (*
  | Block cmds ->  (* Handle a block of commands *)
    let block_scope = analyze_cmds cmds scope in
    { scope with e = block_scope.e }  (* Variables declared in block do not escape *)
  *)
    
  | While (b, c1) ->  (* Handle while loops *)
    let bool_scope = analyze_bool_expr b scope in
    let cmd_scope = analyze_cmd c1 scope in
    { scope with e = bool_scope.e || cmd_scope.e }
  
  | IfThenElse (b, c1, c2) ->  (* Handle if-then-else *)
    let bool_scope = analyze_bool_expr b scope in
    let scope1 = analyze_cmd c1 scope in
    let scope2 = analyze_cmd c2 scope in
    { scope with e = bool_scope.e || scope1.e || scope2.e }
  
  | Parallel (c1, c2) ->  (* Handle parallel commands *)
    let scope1 = analyze_cmd c1 scope in
    let scope2 = analyze_cmd c2 scope in
    { e = scope1.e || scope2.e; v = List.append scope1.v scope2.v; u = List.append scope1.u scope2.u }
  
  | Atom c1 ->  (* Handle atomic commands *)
    let cmd_scope = analyze_cmd c1 scope in
    { scope with e = cmd_scope.e }

  | Call (e1, e2) ->
    (* Analyze the function and arguments *)
    let func_scope = analyze_expr e1 scope in
    let arg_scope = analyze_expr e2 scope in
    (* The error flag should be true if either function or argument has an error *)
    { scope with e = func_scope.e || arg_scope.e }
  
  | Malloc x ->  (* Handle memory allocation *)
    { scope with e = not (List.mem x scope.v) }

  | FieldAssignExpression (e1, e2, e3) ->  (* Handle field assignment *)
    let scope1 = analyze_expr e1 scope in
    let scope2 = analyze_expr e2 scope in
    let scope3 = analyze_expr e3 scope in
    { scope with e = scope1.e || scope2.e || scope3.e }

and analyze_cmds cmds scope print_scope =
  match cmds with
  | [] -> scope
  | cmd :: rest ->
    let cmd_scope = analyze_cmd cmd scope in
    if print_scope then begin
      let buffer = Buffer.create 100 in
      let fmt = formatter_of_buffer buffer in

      print_cmd fmt 0 cmd;
      pp_print_flush fmt ();  (* Flush the formatter *)
      Printf.printf "Before analyzing: %s\n" (Buffer.contents buffer);
      Printf.printf "Current scope: { v = [%s]; e = %b }\n"
                    (String.concat "; " scope.v) scope.e;

      Printf.printf "After analyzing: %s\n" (Buffer.contents buffer);
      Printf.printf "Updated scope: { v = [%s]; e = %b }\n"
                    (String.concat "; " cmd_scope.v) cmd_scope.e;
    end;
    analyze_cmds rest { cmd_scope with e = cmd_scope.e || scope.e } print_scope

let report_unused_variables scope =
  let declared = scope.v in
  let used = scope.u in
  let unused = List.filter (fun var -> not (List.mem var used)) declared in
  List.iter (fun var -> Printf.printf "Warning: Variable '%s' declared but not used.\n" var) unused
    
let run_analysis cmds ~print_scope =
  let initial_scope = { v = []; u = []; e = false } in
  let final_scope = analyze_cmds cmds initial_scope print_scope in
  report_unused_variables final_scope;
  final_scope