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
  | Minus (e1, e2) | Plus (e1, e2) | FieldValue (e1, e2) ->
    let scope1 = analyze_expr e1 scope in
    let scope2 = analyze_expr e2 { scope with v = scope1.v } in
    { scope with e = scope1.e || scope2.e }  (* Propagate error flag *)
  | Variable x -> { scope with u = x :: scope.u; e = not (List.mem x scope.v) }  (* Set error flag if variable not declared *)
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
  
  | Skip -> scope
  
  | Block cmds ->
    let block_scope = analyze_cmds cmds scope false in
      { scope with 
        u = List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc) scope.u block_scope.u;
        e = block_scope.e || scope.e
      }

  | While (b, c1) ->
    let bool_scope = analyze_bool_expr b scope in
    let cmd_scope = analyze_cmd c1 scope in
    { scope with e = bool_scope.e || cmd_scope.e }
  
  | IfThenElse (b, c1, c2) ->
    let bool_scope = analyze_bool_expr b scope in
    let scope1 = analyze_cmd c1 scope in
    let scope2 = analyze_cmd c2 scope in
    { scope with e = bool_scope.e || scope1.e || scope2.e }
  
  | Parallel (c1, c2) -> 
    let scope1 = analyze_cmd c1 scope in
    let scope2 = analyze_cmd c2 scope in
    let merged_v = List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc) scope.v (List.append scope1.v scope2.v) in
    let merged_u = List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc) scope.u (List.append scope1.u scope2.u) in
    { e = scope1.e || scope2.e; v = merged_v; u = merged_u }

  | Atom c1 ->  (* Handle atomic commands *)
    let cmd_scope = analyze_cmd c1 scope in
    { scope with e = cmd_scope.e }

  | Call (e1, e2) ->
    (* Analyze the function and arguments *)
    let func_scope = analyze_expr e1 scope in
    let arg_scope = analyze_expr e2 scope in
    (* The error flag should be true if either function or argument has an error *)
    { scope with e = func_scope.e || arg_scope.e }
  
  | Malloc x -> { scope with e = not (List.mem x scope.v) }

  | FieldAssignExpression (e1, e2, e3) ->
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
      Printf.printf "Analyzing: %s" (Buffer.contents buffer);
      Printf.printf "Current scope: { v = [%s]; e = %b }\n"
                    (String.concat "; " scope.v) scope.e;

      Printf.printf "Updated scope: { v = [%s]; e = %b }\n\n"
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