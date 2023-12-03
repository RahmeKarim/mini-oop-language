(* File: operational_semantics.ml *)

open Semantic_domain
open Pretty_printer
open Parser
open Ast

exception RuntimeError of string

let rec convert_cmd_to_control (cmd: Ast.cmd) : Semantic_domain.control =
  match cmd with
  | Ast.Var x -> Semantic_domain.VarCmd x
  | Ast.Assign (x, e) -> Semantic_domain.AssignCmd (x,  e)
  | Ast.Malloc x -> Semantic_domain.MallocCmd x
  | Ast.Skip -> Semantic_domain.SkipCmd
  | Ast.Block cmds -> Semantic_domain.BlockCmd (List.map convert_cmd_to_control cmds)
  | Ast.While (b, c) -> Semantic_domain.WhileCmd (b, convert_cmd_to_control c)
  | Ast.IfThenElse (b, c1, c2) -> Semantic_domain.IfThenElseCmd (b, convert_cmd_to_control c1, convert_cmd_to_control c2)
  | Ast.FieldAssignExpression (e1, e2, e3) -> Semantic_domain.FieldAssignExpressionCmd ( e1,  e2,  e3)
  | Ast.Parallel (c1, c2) -> Semantic_domain.ParallelCmd (convert_cmd_to_control c1, convert_cmd_to_control c2)
  | Ast.Atom c -> Semantic_domain.AtomCmd (convert_cmd_to_control c)
  | Ast.Call (e1, e2) -> Semantic_domain.CallCmd ( e1,  e2)

let rec convert_cmds_to_control (cmds: Ast.cmds) : Semantic_domain.control list =
  List.map convert_cmd_to_control cmds

let string_of_location loc =
  match loc with
  | ObjectLoc (Object n) -> "Object(" ^ string_of_int n ^ ")"
  | Null -> "Null"

let string_of_value (value: value) =
  match value with
  | Field f -> "Field(" ^ f ^ ")"
  | Int n -> "Int(" ^ string_of_int n ^ ")"
  | Loc l -> "Loc(" ^ string_of_location l ^ ")"
  | Clo c -> "Closure"
  | Null -> "Null"

let string_of_tainted_value (tv: tainted_value) =
  match tv with
  | Val v -> "Val(" ^ string_of_value v ^ ")"
  | Error -> "Error"

let string_of_heap heap =
  Hashtbl.fold (fun (loc, field) tv acc -> 
    acc ^ "(" ^ string_of_location loc ^ ", " ^ field ^ ") -> " ^ string_of_tainted_value tv ^ "; ") heap ""

let string_of_environment env =
  List.fold_left (fun acc (var, loc) -> 
    acc ^ var ^ " -> " ^ string_of_location loc ^ "; ") "" env

let string_of_frame frame =
  match frame with
  | Decl env -> "Decl {" ^ string_of_environment env ^ "}"
  | Call (env, _) -> "Call {" ^ string_of_environment env ^ "}"

let string_of_stack stack =
  "[" ^ 
  (List.fold_right (fun frame acc -> string_of_frame frame ^ " " ^ acc) stack "") ^ 
  "]"
  
let string_of_state state =
  "\nStack: " ^ string_of_stack state.stack ^ "\n" ^
  "Heap: " ^ string_of_heap state.heap

let rec string_of_command cmd =
  match cmd with
  | VarCmd x -> "VarCmd " ^ x
  | AssignCmd (x, e) -> "AssignCmd (" ^ x ^ ", " ^ string_of_expr e ^ ")"
  | BlockCmd cmds -> "BlockCmd [" ^ String.concat ", " (List.map string_of_command cmds) ^ "]"
  | _ -> raise (RuntimeError "Unimplemented")

and string_of_expr (expr: Ast.expr) : string =
  match expr with
  | Field f -> "Field " ^ f
  | Number n -> "Number " ^ string_of_int n
  | Null -> "Null"
  | Minus (e1, e2) -> "Minus (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | FieldValue (e1, e2) -> "FieldValue (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Variable x -> "Variable " ^ x
  | Proc (param, body) -> "Proc (" ^ param ^ ", " ^ string_of_command (convert_cmd_to_control body) ^ ")"

let rec convert_ast_expr_to_semantic_value (expr: Ast.expr) : Semantic_domain.tainted_value =
  match expr with
  | Ast.Field f -> Semantic_domain.Val (Semantic_domain.Field f)
  | Ast.Number n -> Semantic_domain.Val (Semantic_domain.Int n)
  | Ast.Null -> Semantic_domain.Val (Semantic_domain.Loc Semantic_domain.Null)
  | Ast.Minus (e1, e2) ->
      (* Handle the Minus case - you'll need to define how to handle this *)
      Semantic_domain.Error  (* Placeholder *)
  | Ast.FieldValue (e1, e2) ->
      (* Handle FieldValue case *)
      Semantic_domain.Error  (* Placeholder *)
  | Ast.Variable x ->
      (* Handle Variable case *)
      Semantic_domain.Error  (* Placeholder *)
  | Ast.Proc (param, body) ->
      (* Handle Procedure case *)
      Semantic_domain.Error  (* Placeholder *)

(* Helper function to generate a unique location for the heap *)
let location_counter = ref 0

let new_location () =
  let loc = ObjectLoc (Object (!location_counter)) in
  incr location_counter;  (* Increment the counter *)
  loc

(* Helper function to update the heap *)
let update_heap heap loc value =
  Hashtbl.add heap loc value;
  heap

(* Function to evaluate expressions *)
let rec eval_expr expr (state: state) : tainted_value * state =
  match expr with
  | Field f -> (Val (Field f), state)
  | Number n   -> (Val (Int n), state)
  | Null   -> (Val (Loc Null), state)
  | Proc (s, c)   -> (Val (Clo (Closure (s, c, state.stack))), state)
  | _ -> raise (RuntimeError "Unrecognized expression")

let rec eval_cmd cmd (state: state) : state =
  Printf.printf "\n\nEvaluating command: %s" (string_of_command cmd);
  Printf.printf "%s" (string_of_state state);
  let new_state = match cmd with
    | VarCmd x ->
      let loc = new_location () in
      let heap' = update_heap state.heap (loc, "val") (Val Null) in
      let env' = [(x, loc)] in
      { stack = Decl env' :: state.stack; heap = heap' }
    | AssignCmd (x, e) ->
      let (value, state') = eval_expr e state in
      begin match value with
      | Val v ->
        (* Find the location associated with x in the current environment *)
        let loc = match state'.stack with
                  | Decl env :: _ -> List.assoc x env
                  | _ -> raise (RuntimeError "Variable not declared") in
        let heap' = update_heap state'.heap (loc, x) value in
        { state' with heap = heap' }
      | Error -> raise (RuntimeError "Error during assignment")
      end
    | BlockCmd cmds -> eval_block cmds state
    | _ -> raise (RuntimeError "Unrecognized command")
  in
  Printf.printf "\n\nAfter command: %s" (string_of_command cmd);
  Printf.printf "%s" (string_of_state new_state);
  new_state

and eval_block cmds state =
  match cmds with
  | [] -> state
  | cmd :: rest_cmds ->
    let state' = eval_cmd cmd state in
    eval_block rest_cmds state'

(* Evaluate a program with an initial state *)
let eval_prog prog =
  let initial_state = { stack = []; heap = Hashtbl.create 100 } in
  let final_state = eval_block prog initial_state in
  Printf.printf "\n\nFinal state after program execution: %s\n" (string_of_state final_state);
  Printf.printf ("\n");
  final_state