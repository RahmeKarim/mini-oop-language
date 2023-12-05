open Ast

type int_val = int

and bool_val = True | False | Error

and object_val = Object of int

and location = ObjectLoc of object_val | Null

and environment = (string * location) list

and frame = 
  | Decl of environment
  | Call of environment * stack

and stack = frame list

and closure = Closure of string * cmd * stack

and value = 
  | Field of string
  | Int of int_val
  | Loc of location
  | Clo of closure
  | Null  (* Otherwise we always need to use (Loc Null) *)

and tainted_value = 
  | Val of value
  | Error

and heap = (location * string, tainted_value) Hashtbl.t

and state = {
  stack: stack;
  heap: heap;
}

and control =
  | VarCmd of string
  | ExprCmd of expr
  | MallocCmd of string
  | AssignCmd of string * expr
  | SkipCmd
  | BlockCmd of control list
  | WhileCmd of bool_expr * control
  | IfThenElseCmd of bool_expr * control * control
  | FieldAssignExpressionCmd of expr * expr * expr
  | ParallelCmd of control * control
  | AtomCmd of control
  | PopBlock of control list
  | CallCmd of expr * expr

and configuration =
  | CtrlState of control * state
  | State of state
  | Error