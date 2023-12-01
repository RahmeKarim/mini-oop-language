type expr =
  | Field of string
  | Number of int
  | Minus of expr * expr
  | Null
  | Variable of string
  | Proc of string * cmd
  | FieldValue of expr * expr

and cmd =
  | Var of string
  | Assign of string * expr
  | Malloc of string
  | Skip
  | Block of cmds
  | While of bool_expr * cmd
  | IfThenElse of bool_expr * cmd * cmd
  | FieldAssignExpression of expr * expr * expr
  | Parallel of cmd * cmd
  | Atom of cmd
  | Call of expr * expr

and cmds = cmd list

and bool_expr =
  | True
  | False
  | Equal of expr * expr
  | Less of expr * expr

and prog = cmds;;