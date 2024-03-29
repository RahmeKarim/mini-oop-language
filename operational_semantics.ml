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
  | VarCmd x -> "Var " ^ x
  | AssignCmd (x, e) -> "Assign (" ^ x ^ ", " ^ string_of_expr e ^ ")"
  | BlockCmd cmds -> "Block [" ^ String.concat ", " (List.map string_of_command cmds) ^ "]"
  | MallocCmd x -> "Malloc (" ^ x ^ ")"
  | FieldAssignExpressionCmd (e1, e2, e3) -> "FieldAssignExpression (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ", " ^ string_of_expr e3 ^ ")"
  | WhileCmd (cond, cmd) -> "While(" ^ string_of_bool_expr cond ^ ", " ^ string_of_command cmd ^ ")"
  | IfThenElseCmd (cond, cmd1, cmd2) -> "IfThenElse (" ^ string_of_bool_expr cond ^ ", " ^ string_of_command cmd1 ^ ", " ^ string_of_command cmd2 ^ ")"
  | SkipCmd -> "Skip"
  | CallCmd (e1, e2) -> "Call (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | PopBlock cmds -> "PopBlock (" ^ String.concat ", " (List.map string_of_command cmds) ^ ")"
  | ParallelCmd (cmd1, cmd2) -> "ParallelCmd (" ^ string_of_command cmd1 ^ " ||| " ^ string_of_command cmd2 ^ ")"
  | AtomCmd cmd1 -> "atom (" ^ string_of_command cmd1 ^ ")"
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
  
(* Util to collect all fields in AST for malloc *)

let collected_fields = ref []

let rec collect_fields_expr expr =
  match expr with
  | Field f -> 
    if not (List.mem f !collected_fields) then
      collected_fields := f :: !collected_fields
  | FieldValue (e, Field f) ->
    if not (List.mem f !collected_fields) then
      collected_fields := f :: !collected_fields;
    collect_fields_expr e
  | Minus (e1, e2) | Plus (e1, e2) ->
    collect_fields_expr e1;
    collect_fields_expr e2
  | Null | Number _ | Variable _ -> ()
  | Proc (_, body) -> 
    collect_fields_cmd [body]
  | FieldValue (e1, _) ->
    collect_fields_expr e1
  
and collect_fields_cmd cmds =
  match cmds with
  | [] -> ()
  | cmd :: restCmds ->
    begin
      match cmd with
      | Assign (_, e) -> collect_fields_expr e
      | Malloc _ -> ()
      | FieldAssignExpression (e1, e2, e3) -> 
        collect_fields_expr e1;
        collect_fields_expr e2;
        collect_fields_expr e3;
      | Block cmds -> collect_fields_cmd cmds
      | Atom cmd -> collect_fields_cmd [cmd]
      | While (b, c) ->
        collect_fields_bool_expr b;
        collect_fields_cmd [c]
      | IfThenElse (b, c1, c2) ->
        collect_fields_bool_expr b;
        collect_fields_cmd [c1];
        collect_fields_cmd [c2]
      | Parallel (c1, c2) ->
        collect_fields_cmd [c1];
        collect_fields_cmd [c2]
      | Var _ -> ()
      | Skip -> ()
      | Call (e1, e2) ->
        collect_fields_expr e1;
        collect_fields_expr e2
    end;
    collect_fields_cmd restCmds


and collect_fields_bool_expr bool_expr =
  match bool_expr with
  | True | False -> ()
  | Equal (e1, e2) | Less (e1, e2) ->
    collect_fields_expr e1;
    collect_fields_expr e2

    
let reset_collected_fields () =
  collected_fields := []

let print_collected_fields () =
  Printf.printf "Collected fields: [%s]\n" (String.concat ", " !collected_fields)  


(* Actual Operational Semantics *)

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
      | _ -> raise (RuntimeError "Illegal addition")
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
      | _ -> raise (RuntimeError "Illegal field value")
      end

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

(* Function to evaluate a list of commands *)
let rec eval_cmds (cmds: Semantic_domain.control list) (state: Semantic_domain.state)
  : Semantic_domain.state * Semantic_domain.control list =
  match cmds with
  | [] -> state, []  (* No more commands to process, return state and empty command list *)
  | cmd :: rest_cmds ->  (* Process the first command and then the rest *)
      Printf.printf "\n\nEvaluating command: %s" (string_of_command cmd);
      let state', remaining_cmds = eval_cmd cmd rest_cmds state in
      Printf.printf "\n\nAfter command: %s" (string_of_command cmd);
      Printf.printf "%s" (string_of_state state');
      if remaining_cmds = [] then begin
        eval_cmds rest_cmds state'
      end
      else begin
        eval_cmds remaining_cmds state'
      end

(* Function to evaluate a single command and return the remaining commands to continue with *)
and eval_cmd (cmd: Semantic_domain.control) (rest_cmds: Semantic_domain.control list) (state: Semantic_domain.state)
  : Semantic_domain.state * Semantic_domain.control list =
  match cmd with
  | VarCmd x ->
      let loc = new_location () in
      let heap' = update_heap state.heap loc "val" (Val Null) in
      let env' = [(x, loc)] in
      let new_state = { stack = Decl env' :: state.stack; heap = heap' } in
      new_state, [PopBlock rest_cmds]  (* Wrap the remaining commands in PopBlock *)

  | PopBlock cmds_inside_block -> 
    (* Execute all commands within the PopBlock *)
    let state_after_block, _ = eval_cmds cmds_inside_block state in
    (* After executing all commands inside the block, pop the top frame off the stack *)
    begin match state_after_block.stack with
      | _ :: rest_stack -> { state_after_block with stack = rest_stack }, rest_cmds
      | [] -> raise (RuntimeError "Stack underflow on PopBlock")
    end

  | AssignCmd (x, e) ->
      let (value, state') = eval_expr e state in
      begin match value with
        | Val v ->
            begin match find_in_stack x state'.stack with
            | Some loc -> 
                let heap' = update_heap state'.heap loc "val" (Val v) in
                { state' with heap = heap' }, rest_cmds
            | None -> raise (RuntimeError ("Variable not declared: " ^ x))
            end
        | Error -> raise (RuntimeError "Error during assignment")
      end

  | MallocCmd x ->
      let obj_loc = new_location() in
      begin match find_in_stack x state.stack with
      | Some x_loc ->
          (* Initialize all fields collected in the program to Null *)
          let updated_heap = List.fold_left (fun acc_heap field -> 
              update_heap acc_heap obj_loc field (Val Null)
          ) state.heap !collected_fields in
          let heap' = update_heap updated_heap x_loc "val" (Val (Loc obj_loc)) in
          { state with heap = heap' }, rest_cmds
      | None ->
          raise (RuntimeError "Variable not declared")
      end

  | FieldAssignExpressionCmd (e1, e2, e3) -> 
    let (loc_tv, state')  = eval_expr e1 state in
    let (field_tv, state'') = eval_expr e2 state' in
    begin match (loc_tv, field_tv) with 
      | (Val (Loc loc), Val (Field field)) ->
        let (value, state''') = eval_expr e3 state'' in
        begin match value with
        | Val _ ->
            let heap' = update_heap state'''.heap loc field value in
            { state with heap = heap' }, rest_cmds
        | Error ->
            raise (RuntimeError "Error during field assignment")
        end
    | _ ->
        raise (RuntimeError "Invalid types in field assignment")
    end
    
  | WhileCmd (cond, cmd) ->
    begin match eval_bool_expr cond state with
    | True ->
        (* First, evaluate the command associated with the while loop *)
        let state', _ = eval_cmd cmd [] state in
        (* Then, re-evaluate the while loop with the updated state *)
        eval_cmds (WhileCmd(cond, cmd) :: rest_cmds) state'
    | False -> state, rest_cmds
    | Error -> raise (RuntimeError "Error in while condition")
    end

  | IfThenElseCmd (bool_expr, cmd1, cmd2) ->
      begin match eval_bool_expr bool_expr state with
      | True -> eval_cmds (cmd1 :: rest_cmds) state
      | False -> eval_cmds (cmd2 :: rest_cmds) state
      | Error -> raise (RuntimeError "Bool error occurred in if-then-else")
      end

  | SkipCmd -> state, rest_cmds

  | BlockCmd cmds -> eval_cmds cmds state

  | ParallelCmd (cmd1, cmd2) ->
    if Random.bool() then
      let state', remaining_cmds1 = eval_cmds [cmd1] state in
      if remaining_cmds1 = [] then
        let state'', _ = eval_cmds [cmd2] state' in
        state'', rest_cmds
      else
        let new_parallel = ParallelCmd (BlockCmd remaining_cmds1, cmd2) in
        state', new_parallel :: rest_cmds
    else
      let state', remaining_cmds2 = eval_cmds [cmd2] state in
      if remaining_cmds2 = [] then
        let state'', _ = eval_cmds [cmd1] state' in
        state'', rest_cmds
      else
        let new_parallel = ParallelCmd (cmd1, BlockCmd remaining_cmds2) in
        state', new_parallel :: rest_cmds

  | AtomCmd cmd -> eval_cmds [cmd] state

  | CallCmd (e1, e2) ->
    let (v, state') = eval_expr e1 state in
    begin match v with
    | Val(Clo(Closure (x, body, closure_stack))) ->
        let (value, state'') = eval_expr e2 state' in
        let loc = new_location () in
        let heap' = update_heap state''.heap loc "val" value in
        let env = [(x, loc)] in 
        let stack' = Semantic_domain.Call (env, state'.stack) :: closure_stack in
        let state_with_new_frame = { stack = stack'; heap = heap' } in
        (* Evaluate the body of the closure *)
        let state_after_body, _ = eval_cmds [convert_cmd_to_control body] state_with_new_frame in
        (* Restore the stack to its state before the call, keeping the updated heap *)
        let state_restored = { state_after_body with stack = state'.stack } in
        state_restored, rest_cmds
    | _ -> raise (RuntimeError "Called expression did not evaluate to a closure")
    end

  | _ -> raise (RuntimeError "Unrecognized command")

(* Evaluate a program with an initial state *)
let eval_prog prog =
  reset_location_counter ();
  reset_collected_fields ();
  collect_fields_cmd prog;
  print_collected_fields ();
  let initial_state = { stack = []; heap = Hashtbl.create 100 } in
  let (final_state, _) = eval_cmds (convert_cmds_to_control prog) initial_state in
  Printf.printf "\n\nFinal state after program execution: %s\n" (string_of_state final_state);
  Printf.printf "\n";
  final_state
