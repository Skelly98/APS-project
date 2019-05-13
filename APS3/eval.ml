open Ast

type valeur = InN of int (* valeurs immédiates*) 
            | InF of expr * string list * env (* fermeture *)
            | InFR of string * expr * string list * env (* fermeture récursive*)
            | InA of int (* adresse *)
            | InP of block * string list * env (* fermeture procédurale*)
            | InPR of string * block * string list * env (* fermeture récursive*)
            | InB of valeur * int (* bloc mémoire*)
            | Epsilon (* valeur vide *)

and env  = (string * valeur) list 

type memoire = (valeur * valeur) list

let adress_id = ref 0 (* incrémente *)

let size = ref []

let id = ref (InA(-1))

let rec get_size name tab =
  match tab with
  [] -> failwith("id not found")
  |(x,n):: t -> if name = x then n else get_size name t 

let int_of_address a = 
  match a with 
  InA(i) -> i
  |_ ->failwith("not a valid adress")

  
let alloc mem = (InA(!adress_id),InN(-42))::mem (* -42 par défaut *)

let rec allocn_aux a mem i  =
      if i > 0
       then let alloc_mem = (InA(a+i-1),InN(-42))::mem in
            allocn_aux a (alloc_mem) (a+i-1)
      else mem


(*prend une mémoire et un entier en paramètre*)
let rec allocn mem n =
  if n < 0 
    then failwith("size must superior to 0")
    else
    let a = !adress_id in
    let res = allocn_aux a mem n in
    adress_id := !adress_id+n;
    (InA(a),res)
    
    
let rec value_of_ina mem a =
  match mem with 
  [] -> failwith("address undefined : value : "^(string_of_int (int_of_address a)))
  |(InA(i),v) ::t ->  if (int_of_address a) = i 
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

let rec eval_expr e env mem out =
  match e with
    ASTNum n -> (InN(n),mem,out)
    | ASTTrue -> (InN(1),mem,out)
    | ASTFalse -> (InN(0),mem,out)
    | ASTId x -> (match (inEnv x env) with 
                  InA i -> (value_of_ina mem (InA(i)),mem,out)
                  |InB(a,n) -> size := (x,n)::(!size) ;(a,mem,out)
                  | v ->  (v,mem,out)
    )
    | ASTPrim (op, e1, e2) -> (
        eval_op op e1 e2 env mem out
      )
    | ASTUnary (op, e) -> (
        match op with
          Not -> (
            let (val_expr,mem_expr,out_expr) = eval_expr e env mem out in
            if int_of_value (val_expr) = 1 then
              (InN(0),mem_expr,out_expr)
            else (InN(1),mem_expr,out_expr)
          )
          |Alloc -> 
            let (val_expr,mem_expr,out_expr) = eval_expr e env mem out in
            let n = int_of_value val_expr in
            let (addr,mem_alloc) = allocn mem_expr n in 
            (InB(addr,n),mem_alloc,out_expr)
          |Len -> 
            (match e with 
              ASTId x -> (InN(get_size x !size),mem,out)
            )
    )
    | ASTIf (cond, e1, e2) -> (
      let (cond_val,cond_mem,cond_out) = eval_expr cond env mem out in
        if int_of_value(cond_val) = 1 then
          eval_expr e1 env cond_mem cond_out
        else
          eval_expr e2 env cond_mem cond_out
    )
    | ASTAbs (args, e) -> (
        (InF(e,(getNameArgs [] args),env),mem,out)
    )

    | ASTApp (e, es) -> ( 
      let (val_e,val_mem,val_out) = eval_expr e env mem out in
      let (vals_list,mem_n,out_n) = eval_exprs es [] env val_mem val_out in
      match val_e with
          InF (e,param,env_) ->  (
            eval_expr e (List.append (List.combine param vals_list) env_) mem_n out_n
          )
          |InFR(nom,e,param,env_)->  (
            eval_expr e (List.append (List.combine param vals_list)  ((nom,(inEnv nom env))::env_ ) ) mem_n out_n
          )
          |InP(bk,param,env_ ) -> (
            let (res_v,res_mem,res_out) = eval_block bk (List.append (List.combine param vals_list) env_) mem_n out_n in
              (res_v,(!id,res_v)::res_mem,res_out)
          )
          |InPR(name,bk,param,env_) -> (
            let (res_v,res_mem,res_out) = eval_block bk (List.append (List.combine param vals_list) ((name,(inEnv name env))::env_)) mem_n out_n in
            (res_v,(!id,res_v)::res_mem,res_out)
          )
           |_ -> failwith("pas une fonction, ni une procédure")
          )
    

and eval_exprs exprs l env mem out =
    match exprs with
    ASTExpr e -> let (v1,final_mem,final_out) = eval_expr e env mem out in (v1::l,final_mem,final_out)
    |ASTExprs_ (e,ex) -> let (v,new_mem,new_out) = eval_expr e env mem out in 
                        eval_exprs ex (v::l) env new_mem new_out

and eval_op op e1 e2 env mem out =
 
  match op with
    Add -> 
      let (v1,m1,o1) = eval_expr e1 env mem out in 
      let (v2,m2,o2) = eval_expr e2 env m1 o1 in
      (InN(int_of_value (v1) + int_of_value(v2)),m2,o2)
    | Mul ->
      let (v1,m1,o1) = eval_expr e1 env mem out in 
      let (v2,m2,o2) = eval_expr e2 env m1 o1 in
      (InN(int_of_value (v1) * int_of_value(v2)),m2,o2)
    | Sub ->
      let (v1,m1,o1) = eval_expr e1 env mem out in 
      let (v2,m2,o2) = eval_expr e2 env m1 o1 in
      (InN(int_of_value (v1) - int_of_value(v2)),m2,o2)
    | Div ->
      let (v1,m1,o1) = eval_expr e1 env mem out in 
      let (v2,m2,o2) = eval_expr e2 env m1 o1 in
      (InN(int_of_value (v1) / int_of_value(v2)),m2,o2)
    | Or -> (
      let (v1,m1,o1) = eval_expr e1 env mem out in 
      let (v2,m2,o2) = eval_expr e2 env m1 o1 in
        if int_of_value (v1) + int_of_value(v2) > 0 then
          (InN(1),m2,o2)
        else (InN(0),m2,o2)
    )
    | And -> (
        let (v1,m1,o1) = eval_expr e1 env mem out in 
        let (v2,m2,o2) = eval_expr e2 env m1 o1 in
        if int_of_value (v1) + int_of_value(v2) = 2 then 
          (InN(1),m2,o2)
        else (InN(0),m2,o2)
    )
    | Eq -> (
       let (v1,m1,o1) = eval_expr e1 env mem out in 
       let (v2,m2,o2) = eval_expr e2 env m1 o1 in
      if int_of_value (v1) = int_of_value(v2) then
        (InN(1),m2,o2)
      else (InN(0),m2,o2)
    )
    | Lt -> (
      let (v1,m1,o1) = eval_expr e1 env mem out in 
      let (v2,m2,o2) = eval_expr e2 env m1 o1 in
      if int_of_value (v1) < int_of_value(v2) then 
        (InN(1),m2,o2)
      else (InN(0),m2,o2)
    )
    | Nth -> (
      match eval_expr e1 env mem out with
          (InA(a),m1,o1) -> (match eval_expr e2 env m1 o1  with 
                        (InN(i),m2,o2) -> (value_of_ina m2 (InA(i+a)),m2,o2)
          )
          |(InN(a),m1,o1)-> (match eval_expr e2 env m1 o1 with 
                          (InN(i),m2,o2) ->(value_of_ina m2 (InA(i)),m2,o2)
          )
      )
    


 
and eval_stat s env mem out =
    match s with
    ASTEcho e -> ( match eval_expr e env mem out with
                   (InN(n),m,o) -> print_int n; print_string "\n"; (Epsilon,m,out^"")
                   |_ -> failwith "ne s'applique que sur les entiers")
    |ASTSet(lval,e) ->  let (eval_e,emem,o1) = eval_expr e env mem out in
                        let (addr,vmem,o2) = eval_lval lval env emem o1 in 
                        (Epsilon,((InA(addr)),eval_e)::vmem,o2)
    |ASTIF(e,bk1,bk2) ->  let (eval_e,emem,o1) = eval_expr e env mem out in
                          if int_of_value (eval_e) = 1 
                          then eval_block bk1 env emem o1
                          else eval_block bk2 env emem o1
    |ASTWhile(e,bk) ->  let (eval_e,emem,o) = eval_expr e env mem out in
                        if int_of_value (eval_e) = 1 
                            then let (v,mem_,out_)= eval_block bk env emem o in
                            (match v with
                              Epsilon -> eval_stat s env mem_ out_
                              |_ -> (v,mem_,out_)
                            )
                            else (Epsilon,emem,o)

    |ASTCall(name,es)->  let (vals_list,v_mem,v_out) = eval_exprs es [] env mem out in
                          let val_ = inEnv name env in 
                          (match val_ with
                          InP(bk,param,env_ ) -> eval_block bk (List.append (List.combine param vals_list) env_) v_mem v_out
                          |InPR(name,bk,param,env_) -> 
                           eval_block bk (List.append (List.combine param vals_list) ((name,(inEnv name env))::env_)) v_mem v_out
                           |_ -> failwith "not a procedure"
                          )

and eval_lval lval env mem out = 
  
  match lval with
   ASTLvalId name -> let v = inEnv name env in 
                    (match v with
                    InA(a) -> (a,mem,out)
                    |InB(a,n) ->((int_of_address a),mem,out) 
                    )
  | ASTLvalNth(lv,e)-> 
    let (a,mem_lv,o) = eval_lval lv env mem out in
    let (eval_e,mem_e,o1) = eval_expr e env mem_lv o in
    (match eval_e with
      InN(i) -> ((a+i),mem_e,o1)
    )

and eval_dec d env mem out = 
    match d with
      ASTConst(name, t, e) -> let (eval_e,emem,eout) = eval_expr e env mem out in
                            ((name,(eval_e))::env,emem,eout)
      |ASTFun(name, t, a, e) ->  ((name,InF(e,(getNameArgs [] a),env))::env,mem,out)
      |ASTFunRec(name, t, a, e) ->((name,InFR(name, e, (getNameArgs [] a),env))::env,mem,out)
      |ASTVar(name,t) -> let mem_ = alloc mem in
                          let (x,y) = ((name,InA(!adress_id))::env,mem_) in
                          adress_id := !adress_id +1;
                          (x,y,out)
      |ASTProc(name,a,bk) -> ((name,InP(bk,(getNameArgs [] a),env))::env,mem,out)
      |ASTProcRec(name,a,bk) -> ((name,InPR(name, bk, (getNameArgs [] a),env))::env,mem,out)
      |ASTFunRet (name, t, a, bk) -> ((name,InP(bk,(getNameArgs [] a),env))::env,mem,out)
      |ASTFunRecRet (name, t, a, bk) -> ((name,InPR(name, bk, (getNameArgs [] a),env))::env,mem,out)
                        
                          

and eval_cmds cs env mem out= 
    match cs with
      ASTStat s -> eval_stat s env mem out
      |ASTDecCmds (d , cmds) -> (
        let (newEnv,newMem,newOut) = eval_dec d env mem out in
          eval_cmds cmds newEnv newMem newOut
      )
      |ASTStatCmds (s , cmds) -> (
          let (v,newMem,newOut)= eval_stat s env mem out in
          eval_cmds cmds env newMem newOut
      )
      |ASTRetCmds r -> eval_ret r env mem out
      |_ ->(Epsilon,mem,out)

and eval_ret r env mem out =
    match r with
    ASTReturn e -> eval_expr e env mem out

and eval_block bk env mem out = 
  match bk with
  ASTBlock cmds -> eval_cmds cmds env mem out

and eval_prog p env mem out =
  match p with
    ASTProg cmds -> eval_cmds cmds env mem out



let rec eval_list = function 
[] -> exit 0
| e::l ->   let oc = open_in ("exemple/"^e) in 
            let lexbuf = Lexing.from_channel oc in
            let p = Parser.prog Lexer.token lexbuf in
              print_string e;
              print_char '\n';
              adress_id := 0;
              eval_prog p [] [] "";
              print_char '\n';
              eval_list l

let _ = 
  let arr = Sys.readdir "exemple" in
    eval_list (Array.to_list arr)
        
     