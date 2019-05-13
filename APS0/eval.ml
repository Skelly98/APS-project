open Ast

type valeur = InN of int | InF of expr * string list * env | InFR of string * expr * string list * env
    
and env  = (string * valeur) list 

let int_of_value v =
  match v with
    InN v -> v


let rec print_list = function 
[] -> ()
| (x,_)::l ->  Printf.printf "%s\n" x ;print_list l


let rec inEnv x env =
  match env with
  [] -> failwith ("l'id "^x^" n'existe pas")
  |(name,v)::t -> if name = x then v
                    else inEnv x t 

let rec getNameArgs l args = 
  match args with
  ASTArg(a) -> getNameArg a l
  |ASTArgs(a,args) -> (getNameArgs (getNameArg a l) args)
and getNameArg a l =
  match a with 
  ASTColon(s,t) -> s::l

let rec eval_expr e env =
  match e with
    ASTNum n -> InN(n)
    | ASTTrue -> InN(1)
    | ASTFalse -> InN(0)
    | ASTId x -> inEnv x env
    | ASTPrim (op, e1, e2) -> (
        eval_op op e1 e2 env
      )
    | ASTUnary (op, e) -> (
        match op with
          Not -> (      (*on a que not pour l'instant*)
            if int_of_value (eval_expr e env) = 1 then
              InN(0)
            else InN(1)
          )
    )
    | ASTIf (cond, e1, e2) -> (
        if int_of_value (eval_expr cond env) = 1 then
          eval_expr e1 env
        else
          eval_expr e2 env
    )
    | ASTAbs (args, e) -> (
        InF(e,(getNameArgs [] args),env)
    )

    | ASTApp (e, es) -> (
      let vals_list = eval_exprs es [] env in
      let eval_e = eval_expr e env in
        match eval_e with
          InF (e,param,env_)->  (
            eval_expr e (List.append (List.combine param vals_list) env_)
          )
          |InFR(nom,e,param,env_) ->  (
                   eval_expr e (List.append (List.combine param vals_list)  ((nom,(inEnv nom env))::env_ ) ) 
          )
          |_ -> failwith "pas une fonction"
    )

    (*recupere list valeur*)
and eval_exprs exprs l env =
    match exprs with
    ASTExpr e -> (eval_expr e env)::l
    |ASTExprs_ (e,ex) -> eval_exprs ex (eval_aux_exprs e l env) env 
and eval_aux_exprs e l env = 
    (eval_expr e env)::l

and eval_op op e1 e2 env =
  match op with
    Add -> InN(int_of_value (eval_expr e1 env) + int_of_value(eval_expr e2 env))
    | Mul -> InN(int_of_value (eval_expr e1 env) * int_of_value(eval_expr e2 env))
    | Sub -> InN(int_of_value (eval_expr e1 env) - int_of_value(eval_expr e2 env))
    | Div -> InN(int_of_value (eval_expr e1 env) / int_of_value(eval_expr e2 env))
    | Or -> (
        if int_of_value (eval_expr e1 env) + int_of_value(eval_expr e2 env) > 0 then (*une des deux expr est vraie et vaut 1, donc >0 en tout*)
          InN(1)
        else InN(0)
    )
    | And -> (
        if int_of_value (eval_expr e1 env) + int_of_value(eval_expr e2 env) = 2 then (*les deux expr sont vraies donc valent 1 donc 2 en tout*)
          InN(1)
        else InN(0)
    )
    | Eq -> (
      if int_of_value (eval_expr e1 env) = int_of_value(eval_expr e2 env) then
        InN(1)
      else InN(0)
    )
    | Lt -> (
      if int_of_value (eval_expr e1 env) < int_of_value(eval_expr e2 env) then 
        InN(1)
      else InN(0)
    )

and eval_stat s env =
    match s with
    ASTEcho e -> ( match eval_expr e env with
                   InN(n) -> print_int n)
                   |_ -> failwith "ne s'applique que sur les entiers"

and eval_dec d env = 
    match d with
      ASTConst(name, t, e) -> (name,(eval_expr e env))::env
|ASTFun(name, t, a, e) ->  (name,InF(e,(getNameArgs [] a),env))::env
|ASTFunRec(name, t, a, e) ->(name,InFR(name, e, (getNameArgs [] a),env))::env

and eval_cmds cs env = 
    match cs with
      ASTStat s -> eval_stat s env
      |ASTDecCmds (d , cmds) -> (
        let newEnv = eval_dec d env in
          eval_cmds cmds newEnv
      )
      |ASTStatCmds (s , cmds) -> (
          eval_stat s env;
          eval_cmds cmds env
      )

and eval_prog p env =
  match p with
    ASTProg cmds -> eval_cmds cmds env

  let _ =
    let oc = open_in Sys.argv.(1) in 
    let lexbuf = Lexing.from_channel oc in
    let p = Parser.prog Lexer.token lexbuf in
    eval_prog p [] ;
    print_char '\n'