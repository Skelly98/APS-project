type op = Add | Mul | Sub | Div | Or | And | Eq | Lt | Not

let string_of_op op =
    match op with
    Add -> "add"
    | Mul -> "mul"
    | Sub -> "sub"
    | Div -> "div"
    | Or -> "or"
    | And -> "and"
    | Eq -> "eq"
    | Lt -> "lt"
    | Not -> "not"

let op_of_string op =
  match op with
  "add" -> Add
  | "mul" -> Mul
  | "sub" -> Sub
  | "div" -> Div
  | "or"  -> Or 
  | "and" -> And
  | "eq" -> Eq
  | "lt" -> Lt
  | "not" -> Not



  type type_ = 
    ASTInt
    |ASTBool
    |ASTArrow of types * type_
  
  and types = 
    ASTType of type_
    |ASTTypes of type_ * types

  type arg = 
    ASTColon of string * type_

  type args = 
    ASTArg of arg
    |ASTArgs of arg * args


   type expr =
    ASTNum of int
    | ASTId of string
    | ASTPrim of op * expr * expr
    | ASTUnary of op * expr 
    | ASTTrue
    | ASTFalse
    | ASTAbs of args * expr 
    | ASTApp of expr * exprs
    | ASTIf of expr * expr * expr
  
  and exprs =
    ASTExpr of expr
    |ASTExprs_ of expr * exprs

  type dec =
      ASTConst of string * type_ * expr
      |ASTFun of string * type_ * args * expr
      |ASTFunRec of string * type_ * args * expr
  
  type stat =
      ASTEcho of expr 
    
  type cmds =
      ASTStat of stat
      |ASTDecCmds of dec * cmds
      |ASTStatCmds of stat * cmds

  type prog = 
      ASTProg of cmds

  (* *)