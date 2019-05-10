open Ast

type valeur = InN of int (* valeurs immédiates*) 
            | InF of expr * string list * env (* fermeture *)
            | InFR of string * expr * string list * env (* fermeture récursive*)
            | InA of int (* adresse *)
            | InP of block * string list * env (* fermeture procédurale*)
            | InPR of string * block * string list * env (* fermeture récursive*)

and env  = (string * valeur) list 

type memoire = (valeur * valeur) list

let adress_id = ref 0
  
let alloc mem = (InA(!adress_id),InN(-42))::mem (* -42 par défaut *)

let int_of_address a = 
  match a with 
  InA(i) -> i
  |_ ->failwith("not a valid adress")
    
let rec value_of_ina mem a =
  match mem with 
  [] -> failwith("address undefined : value : "^(string_of_int (int_of_address a)))
  |(InA(i),v) ::t -> if (int_of_address a) = i 
                      then v
                      else value_of_ina t a 


let rec writeInMem mem a v =
  match mem with
  [] -> failwith("address undefined : write : "^(string_of_int (int_of_address a)))
  |(InA(i),_) :: t -> if  (int_of_address a) = i 
                           then (InA(i),v)
                           else writeInMem t a v             
                           
let int_of_value v =
  match v with
    InN v -> v
    |_ -> failwith("valeurs immediates")
                                
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

let rec eval_expr e env mem =
  match e with
    ASTNum n -> InN(n)
    | ASTTrue -> InN(1)
    | ASTFalse -> InN(0)
    | ASTId x -> (match (inEnv x env) with 
                   InA s -> value_of_ina mem (InA(s))
                  | v -> v
    )
    | ASTPrim (op, e1, e2) -> (
        eval_op op e1 e2 env mem
      )
    | ASTUnary (op, e) -> (
        match op with
          Not -> (
            if int_of_value (eval_expr e env mem) = 1 then
              InN(0)
            else InN(1)
          )
    )
    | ASTIf (cond, e1, e2) -> (
        if int_of_value (eval_expr cond env mem) = 1 then
          eval_expr e1 env mem
        else
          eval_expr e2 env mem
    )
    | ASTAbs (args, e) -> (
        InF(e,(getNameArgs [] args),env)
    )

    | ASTApp (e, es) -> (
      let vals_list = eval_exprs es [] env mem in
      let eval_e = eval_expr e env mem in
        match eval_e with
          InF (e,param,env_)->  (
            eval_expr e (List.append (List.combine param vals_list) env_) mem
          )
          |InFR(nom,e,param,env_) ->  (
                   eval_expr e (List.append (List.combine param vals_list)  ((nom,(inEnv nom env))::env_ ) ) mem 
          )
          |_ -> failwith "pas une fonction"
    )

    (*recupere list valeur*)
and eval_exprs exprs l env mem=
    match exprs with
    ASTExpr e -> (eval_expr e env mem)::l
    |ASTExprs_ (e,ex) -> eval_exprs ex ((eval_expr e env mem)::l) env mem 

and eval_op op e1 e2 env mem=
  match op with
    Add -> InN(int_of_value (eval_expr e1 env mem) + int_of_value(eval_expr e2 env mem))
    | Mul -> InN(int_of_value (eval_expr e1 env mem) * int_of_value(eval_expr e2 env mem))
    | Sub -> InN(int_of_value (eval_expr e1 env mem) - int_of_value(eval_expr e2 env mem))
    | Div -> InN(int_of_value (eval_expr e1 env mem) / int_of_value(eval_expr e2 env mem))
    | Or -> (
        if int_of_value (eval_expr e1 env mem) + int_of_value(eval_expr e2 env mem) > 0 then (*une des deux expr est vraie et vaut 1, donc >0 en tout*)
          InN(1)
        else InN(0)
    )
    | And -> (
        if int_of_value (eval_expr e1 env mem) + int_of_value(eval_expr e2 env mem) = 2 then (*les deux expr sont vraies donc valent 1 donc 2 en tout*)
          InN(1)
        else InN(0)
    )
    | Eq -> (
      if int_of_value (eval_expr e1 env mem) = int_of_value(eval_expr e2 env mem) then
        InN(1)
      else InN(0)
    )
    | Lt -> (
      if int_of_value (eval_expr e1 env mem) < int_of_value(eval_expr e2 env mem) then 
        InN(1)
      else InN(0)
    )

and eval_stat s env mem =
    match s with
    ASTEcho e -> ( match eval_expr e env mem with
                   InN(n) -> print_int n; print_string "\n"; mem
                   |_ -> failwith "ne s'applique que sur les entiers")
    |ASTSet(name,e) -> let val_ = inEnv name env in let eval_e = eval_expr e env mem in
                        (match val_ with 
                          InA(i) -> (writeInMem mem (InA(i)) eval_e)::mem
                          |_ -> failwith (name^" not declared as a variable")
                        )
    |ASTIF(e,bk1,bk2) ->  if int_of_value (eval_expr e env mem) = 1 
                          then eval_block bk1 env mem
                          else eval_block bk2 env mem
    |ASTWhile(e,bk) ->  if int_of_value (eval_expr e env mem) = 1 
                            then let mem_= eval_block bk env mem in
                            eval_stat s env mem_
                            else mem
    |ASTCall(name,es)->  let vals_list = eval_exprs es [] env mem in
                          let val_ = inEnv name env in 
                          (match val_ with
                          InP(bk,param,env_ ) -> eval_block bk (List.append (List.combine param vals_list) env_) mem
                          |InPR(name,bk,param,env_) -> 
                           eval_block bk (List.append (List.combine param vals_list) ((name,(inEnv name env))::env_)) mem
                           |_ -> failwith "not a procedure"
                          )
and eval_dec d env mem = 
    match d with
      ASTConst(name, t, e) -> ((name,(eval_expr e env mem))::env,mem)
      |ASTFun(name, t, a, e) ->  ((name,InF(e,(getNameArgs [] a),env))::env,mem)
      |ASTFunRec(name, t, a, e) ->((name,InFR(name, e, (getNameArgs [] a),env))::env,mem)
      |ASTVar(name,t) -> let mem_ = alloc mem in
                          let (x,y) = ((name,InA(!adress_id))::env,mem_) in
                          adress_id := !adress_id +1;
                          (x,y)
      |ASTProc(name,a,bk) -> ((name,InP(bk,(getNameArgs [] a),env))::env,mem)
      |ASTProcRec(name,a,bk) -> ((name,InPR(name, bk, (getNameArgs [] a),env))::env,mem)
                        
                          

and eval_cmds cs env mem = 
    match cs with
      ASTStat s -> eval_stat s env mem
      |ASTDecCmds (d , cmds) -> (
        let (newEnv,newMem) = eval_dec d env mem in
          eval_cmds cmds newEnv newMem
      )
      |ASTStatCmds (s , cmds) -> (
          let newMem = eval_stat s env mem in
          eval_cmds cmds env newMem
      )

and eval_block bk env mem = 
  match bk with
  ASTBlock cmds -> eval_cmds cmds env mem

and eval_prog p env mem =
  match p with
    ASTProg cmds -> eval_cmds cmds env mem



let rec eval_list = function 
[] -> exit 0
| e::l ->   let oc = open_in ("exemple/"^e) in 
            let lexbuf = Lexing.from_channel oc in
            let p = Parser.prog Lexer.token lexbuf in
              print_string e;
              print_char '\n';
              adress_id := 0;
              eval_prog p [] [];
              print_char '\n';
              eval_list l

let _ = 
  let arr = Sys.readdir "exemple" in
    eval_list (Array.to_list arr)
        
     