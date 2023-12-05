(* File: operational_semantics.ml *)

open Semantic_domain
open Pretty_printer
open Parser
open Ast

exception RuntimeError of string

(* Utils to make AST cmds compatible with Semantic Domain Configuration *)
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

(* Utils to print out configuration at every step of the trace *)
let string_of_location loc =
  match loc with
  | ObjectLoc (Object n) -> "Object(" ^ string_of_int n ^ ")"
  | Null -> "Null"

let rec string_of_command cmd =
  match cmd with
  | VarCmd x -> "VarCmd " ^ x
  | AssignCmd (x, e) -> "AssignCmd (" ^ x ^ ", " ^ string_of_expr e ^ ")"
  | BlockCmd cmds -> "BlockCmd [" ^ String.concat ", " (List.map string_of_command cmds) ^ "]"
  | MallocCmd x -> "MallocCmd (" ^ x ^ ")"
  | FieldAssignExpressionCmd (e1, e2, e3) -> "FieldAssignExpressionCmd (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ", " ^ string_of_expr e3 ^ ")"
  | WhileCmd (cond, cmd) -> "WhileCmd (" ^ string_of_bool_expr cond ^ ", " ^ string_of_command cmd ^ ")"
  | IfThenElseCmd (cond, cmd1, cmd2) -> "IfThenElseCmd (" ^ string_of_bool_expr cond ^ ", " ^ string_of_command cmd1 ^ ", " ^ string_of_command cmd2 ^ ")"
  | SkipCmd -> "SkipCmd"
  | CallCmd (e1, e2) -> "CallCmd (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | PopBlock cmd -> "PopBlock (" ^ string_of_command cmd ^ ")"
  | _ -> raise (RuntimeError "Unimplemented")

and string_of_bool_expr (bool_expr: Ast.bool_expr) : string =
  match bool_expr with
  | True -> "True"
  | False -> "False"
  | Equal(e1, e2) -> "Equal (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Less(e1, e2) -> "Less (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  
and string_of_expr (expr: Ast.expr) : string =
  match expr with
  | Field f -> "Field " ^ f
  | Number n -> "Number " ^ string_of_int n
  | Null -> "Null"
  | Minus (e1, e2) -> "Minus (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Plus (e1, e2) -> "Plus (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | FieldValue (e1, e2) -> "FieldValue (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Variable x -> "Variable " ^ x
  | Proc (param, body) -> "Proc (" ^ param ^ ", " ^ string_of_command (convert_cmd_to_control body) ^ ")"
  
and string_of_closure (Closure (param, body, closure_stack)) =
  "Closure(param: " ^ param ^ ", body: " ^ string_of_command (convert_cmd_to_control body) ^ ", env: " ^ string_of_stack closure_stack ^ ")"

and string_of_value (value: value) =
  match value with
  | Field f -> "Field(" ^ f ^ ")"
  | Int n -> "Int(" ^ string_of_int n ^ ")"
  | Loc l -> "Loc(" ^ string_of_location l ^ ")"
  | Clo closure -> string_of_closure closure
  | Null -> "Null"

and string_of_tainted_value (tv: tainted_value) =
  match tv with
  | Val v -> "Val(" ^ string_of_value v ^ ")"
  | Error -> "Error"

and string_of_heap heap =
  Hashtbl.fold (fun (loc, field) tv acc -> 
    acc ^ "(" ^ string_of_location loc ^ ", " ^ field ^ ") -> " ^ string_of_tainted_value tv ^ "; ") heap ""

and string_of_environment env =
  List.fold_left (fun acc (var, loc) -> 
    acc ^ var ^ " -> " ^ string_of_location loc ^ "; ") "" env

and string_of_frame frame =
  match frame with
  | Decl env -> "Decl { " ^ string_of_environment env ^ "}"
  | Call (env, stack) -> "Call { " ^ string_of_environment env ^ "+ stack: " ^ string_of_stack stack ^ "}"

and string_of_stack stack =
  "[" ^ 
  (List.fold_right (fun frame acc -> string_of_frame frame ^ " " ^ acc) stack "") ^ 
  "]"
  
and string_of_state state =
  "\nStack: " ^ string_of_stack state.stack ^ "\n" ^
  "Heap: " ^ string_of_heap state.heap
  
(* Actual Operational Semantics *)

let collected_fields = ref []

let rec collect_fields_expr expr =
  match expr with
  | Field f -> collected_fields := f :: !collected_fields
  | FieldValue (e, Field f) -> 
    collected_fields := f :: !collected_fields;
    collect_fields_expr e
  | Minus (e1, e2) | FieldValue (e1, e2) ->
    collect_fields_expr e1;
    collect_fields_expr e2
  | Plus (e1, e2) | FieldValue (e1, e2) ->
    collect_fields_expr e1;
    collect_fields_expr e2
  | Null | Number _ | Variable _ -> ()
  | Proc (_, body) -> 
    collect_fields_cmd [body]

and collect_fields_cmd cmds =
  match cmds with
  | [] -> ()
  | cmd :: restCmds ->
    begin
      match cmd with
      | Assign (_, e) -> collect_fields_expr e
      | FieldAssignExpression (e1, Field f, e3) -> 
        collected_fields := f :: !collected_fields;
        collect_fields_expr e1;
        collect_fields_expr e3;
      | Block nested_cmds -> collect_fields_cmd nested_cmds
      | _ -> ()
    end;
    collect_fields_cmd restCmds
    
let reset_collected_fields () =
  collected_fields := []

let print_collected_fields () =
  Printf.printf "Collected fields:\n";
  List.iter (fun field -> Printf.printf "%s\n" field) !collected_fields

(* Helper function to generate a unique location for the heap *)
let location_counter = ref 0

let reset_location_counter () =
  location_counter := 0

let new_location () =
  let loc = ObjectLoc (Object (!location_counter)) in
  incr location_counter;  (* Increment the counter *)
  loc

(* Helper function to update the heap *)
let update_heap heap loc field value =
  Hashtbl.replace heap (loc, field) value;
  heap

(* Helper function to search for a variable in an environment *)
let rec find_in_environment x = function
  | [] -> None
  | (var, loc) :: rest ->
    if var = x then Some loc
    else find_in_environment x rest

(* Helper function to search for a variable in the stack *)
let rec find_in_stack x = function
  | [] -> None
  | frame :: rest_stack ->
    match frame with
    | Decl env ->
      begin
        match find_in_environment x env with
        | Some loc -> Some loc
        | None -> find_in_stack x rest_stack
      end
    | Call (env, stack') ->
      match find_in_environment x env with
      | Some loc -> Some loc
      | None -> find_in_stack x rest_stack

(* Function to evaluate expressions *)
let rec eval_expr expr (state: state) : tainted_value * state =
  match expr with
  | Field  f    -> (Val (Field f), state)
  | Number n    -> (Val (Int n), state)
  | Variable x ->
    begin match find_in_stack x state.stack with
    | Some loc -> 
      begin match Hashtbl.find_opt state.heap (loc, "val") with
      | Some value -> (value, state)
      | None -> (Error, state)
      end
    | None -> (Error, state)
    end
  | Null        -> (Val (Loc Null), state)
  | Proc (s, c) -> (Val (Clo (Closure (s, c, state.stack))), state)
  | Minus (e1, e2) -> 
    let (v1, state1) = eval_expr e1 state in
    let (v2, state2) = eval_expr e2 state1 in
    begin
      match (v1, v2) with
      | (Val (Int n1), Val (Int n2)) -> (Val (Int (n1 - n2)), state2)
      | _ -> raise (RuntimeError "Illegal subtraction")
    end
  | Plus (e1, e2) -> 
    let (v1, state1) = eval_expr e1 state in
    let (v2, state2) = eval_expr e2 state1 in
    begin
      match (v1, v2) with
      | (Val (Int n1), Val (Int n2)) -> (Val (Int (n1 + n2)), state2)
      | _ -> raise (RuntimeError "Illegal subtraction")
    end
  | FieldValue (e1, e2) ->
    let (loc_tv, state') = eval_expr e1 state in
    let (field_tv, state'') = eval_expr e2 state' in
    begin
      match (loc_tv, field_tv) with
      | (Val (Loc loc), Val (Field field)) ->
        begin match Hashtbl.find_opt state''.heap (loc, field) with
          | Some value -> (value, state'')
          | None -> (Error, state'')
        end
      end
  | _ -> raise (RuntimeError "Unrecognized expression")

(* Function to evaluate expressions *)
let rec eval_bool_expr bool_expr (state: state) : bool_val =
  match bool_expr with
  | True -> True
  | False -> False
  | Equal(e1, e2) ->
    let (v1, state') = eval_expr e1 state in
    let (v2, state'') = eval_expr e2 state' in
    begin match (v1, v2) with
      | (Val (Int n1), Val (Int n2)) -> if n1 = n2 then True else False
      | (Val (Loc l1), Val (Loc l2)) -> if l1 = l2 then True else False
      | _ -> Error
    end
  | Less(e1, e2) ->
    let (v1, state') = eval_expr e1 state in
    let (v2, state'') = eval_expr e2 state' in
    begin match (v1, v2) with
      | (Val (Int n1), Val (Int n2)) -> if n1 < n2 then True else False
      | _ -> Error
    end

(*
| AssignCmd (x, e) ->
  let (value, state') = eval_expr e state in
  begin match value with
  | Val v ->
    begin match find_in_stack x state.stack with
    | Some loc -> 
      begin match Hashtbl.find_opt state.heap (loc, "val") with
        | Some value ->
          let heap' = update_heap state'.heap loc "val" value in
          { state' with heap = heap' }
        | None ->  raise (RuntimeError "Error in finding variable in assignment")
      end
    end
  | Error -> raise (RuntimeError "Error during assignment")
  end
*)

(* Function to evaluate command *)
let rec eval_cmd cmd (state: state) : state =
  Printf.printf "\n\nEvaluating command: %s" (string_of_command cmd);
  Printf.printf "%s" (string_of_state state);
  let new_state = match cmd with
    | VarCmd x ->
      let loc = new_location () in
      let heap' = update_heap state.heap loc "val" (Val Null) in
      let env' = [(x, loc)] in
      { stack = Decl env' :: state.stack; heap = heap' }
    | AssignCmd (x, e) ->
      let (value, state') = eval_expr e state in
      begin match value with
        | Val v ->
          begin match find_in_stack x state'.stack with
          | Some loc -> 
            let heap' = update_heap state'.heap loc "val" (Val v) in
            { state' with heap = heap' }
          | None -> raise (RuntimeError ("Variable not declared: " ^ x))
          end
        | Error -> raise (RuntimeError "Error during assignment")
      end
    | MallocCmd x ->
      let obj_loc = new_location() in
      begin match find_in_stack x state.stack with
      | Some x_loc ->
        let heap' = update_heap state.heap x_loc "val" (Val (Loc obj_loc)) in
        
        (* Initialize all fields collected in the program to Null *)
        List.iter (fun field -> 
          let heap' = update_heap heap' obj_loc field (Val Null) in
          ()
        ) !collected_fields;
        { state with heap = heap' }
      | None ->
        Printf.printf "Variable '%s' not declared!\n" x;
        raise (RuntimeError "Variable not declared")
      end
    | FieldAssignExpressionCmd (e1, e2, e3) -> 
      let (loc_tv, state')  = eval_expr e1 state in
      let (field_tv, state'') = eval_expr e2 state' in
      begin match (loc_tv, field_tv) with 
        | (Val (Loc loc), Val (Field field)) ->
          let (value, state''') = eval_expr e3 state'' in
          begin match Hashtbl.find_opt state'''.heap (loc, field) with
            | Some _ ->
              let heap' = update_heap state'''.heap loc field value in
                { state with heap = heap' }
            | None ->
              raise (RuntimeError "Field not found")
          end
        | _ ->
          raise (RuntimeError "Error during assignment")
      end
    | WhileCmd (cond, cmd) ->
      begin match eval_bool_expr cond state with
        | True  ->  let state' = eval_cmd cmd state in
                      eval_cmd (WhileCmd(cond, cmd)) state'
        | False -> state
        | Error -> raise (RuntimeError "Error in while condition")
      end
    | IfThenElseCmd (bool_expr, cmd1, cmd2) ->
      begin match eval_bool_expr bool_expr state with
        | True  -> eval_cmd cmd1 state
        | False -> eval_cmd cmd2 state
        | Error -> raise (RuntimeError "Bool error occured in if else then")
      end
    | SkipCmd -> state
    | CallCmd (e1, e2) ->
      let (v, state') = eval_expr e1 state in
      begin match v with
        | Val(Clo(Closure (x, body, closure_stack))) ->
          let (value, state'') = eval_expr e2 state' in
          let loc = new_location () in
          let heap' = update_heap state''.heap loc "val" value in
          let env = [(x, loc)] in 
          let stack' = Semantic_domain.Call (env, state.stack) :: closure_stack in
          let state_with_new_frame = { stack = stack'; heap = heap' } in
          let state_after_body = eval_cmd (convert_cmd_to_control body) state_with_new_frame in
          let state_restored = { state_after_body with stack = state.stack } in
            state_restored
        | _ -> raise (RuntimeError "Called expression did not evaluate to a closure")
      end
    
    | PopBlock cmd ->
      let state' = eval_cmd cmd state in
      begin match state'.stack with
        | top_frame :: rest_stack -> { state' with stack = rest_stack }
        | [] -> raise (RuntimeError "Stack underflow on PopBlock: Attempted to pop from an empty stack.")
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
  reset_location_counter ();
  reset_collected_fields ();
  collect_fields_cmd prog;
  print_collected_fields ();
  let initial_state = { stack = []; heap = Hashtbl.create 100 } in
  let final_state = eval_block (convert_cmds_to_control prog) initial_state in
  Printf.printf "\n\nFinal state after program execution: %s\n" (string_of_state final_state);
  Printf.printf "\n";
  final_state
