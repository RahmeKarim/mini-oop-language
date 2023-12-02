(* File: semantic_domains.ml *)

type int_val = int

type bool_val =True | False | Error

type object_val = Object of int

type location = object_val | Null

type stack = frame list

and closure = Closure of string * command * stack

and value = 
  | Field of string
  | Int of int_val
  | Loc of location
  | Clo of closure

and tainted_value = 
  | Val of value
  | Error

type environment = (string * object_val) list

and frame = 
  | Decl of environment
  | Call of environment * stack

type heap = (object_val * string, tainted_value) Hashtbl.t

type state = stack * heap

type control =
  | VarCmd of string * control
  | ExprCmd of expr
  | MallocCmd of string
  | AssignCmd of string * expr
  | SkipCmd
  | BlockCmd of control list
  | WhileCmd of bool_expr * control
  | IfThenElseCmd of bool_expr * control * control
  | ParallelCmd of control * control
  | AtomCmd of control
  | PopBlock of control

type configuration =
  | CtrlState of control * state
  | State of state
  | Error