open Ast

type valeur = InN of int (* valeurs immédiates*) 
            | InF of expr * string list * env (* fermeture *)
            | InFR of string * expr * string list * env (* fermeture récursive*)
            | InA of int (* adresse *)
            | InP of block * string list * env (* fermeture procédurale*)
            | InPR of string * block * string list * env (* fermeture récursive*)
            | InB of valeur * int (* bloc mémoire*)

and env  = (string * valeur) list 

type memoire = (valeur * valeur) list

let adress_id = ref 0 (* incrémente *)

let size = ref []

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

let rec eval_expr e env mem =
  match e with
    ASTNum n -> (InN(n),mem)
    | ASTTrue -> (InN(1),mem)
    | ASTFalse -> (InN(0),mem)
    | ASTId x -> (match (inEnv x env) with 
                  InA i -> (value_of_ina mem (InA(i)),mem)
                  |InB(a,n) -> size := (x,n)::(!size) ;(a,mem)
                  | v ->  (v,mem)
    )
    | ASTPrim (op, e1, e2) -> (
        eval_op op e1 e2 env mem
      )
    | ASTUnary (op, e) -> (
        match op with
          Not -> (
            let (val_expr,mem_expr) = eval_expr e env mem in
            if int_of_value (val_expr) = 1 then
              (InN(0),mem_expr)
            else (InN(1),mem_expr)
          )
          |Alloc -> 
            let (val_expr,mem_expr) = eval_expr e env mem in
            let n = int_of_value val_expr in
            let (addr,mem_alloc) = allocn mem_expr n in 
            (InB(addr,n),mem_alloc)
          |Len -> 
            (match e with 
              ASTId x -> (InN(get_size x !size),mem)
            )
    )
    | ASTIf (cond, e1, e2) -> (
      let (cond_val,cond_mem) = eval_expr cond env mem in
        if int_of_value(cond_val) = 1 then
          eval_expr e1 env cond_mem
        else
          eval_expr e2 env cond_mem
    )
    | ASTAbs (args, e) -> (
        (InF(e,(getNameArgs [] args),env),mem)
    )

    | ASTApp (e, es) -> ( 
      let (val_e,val_mem) = eval_expr e env mem in
      let (vals_list,mem_n) = eval_exprs es [] env val_mem in
      match val_e with
          InF (e,param,env_) ->  (
            eval_expr e (List.append (List.combine param vals_list) env_) mem_n
          )
          |InFR(nom,e,param,env_)->  (
            eval_expr e (List.append (List.combine param vals_list)  ((nom,(inEnv nom env))::env_ ) ) mem_n
          )
          |_ -> failwith "pas une fonction"
    )

and eval_exprs exprs l env mem=
    match exprs with
    ASTExpr e -> let (v1,final_mem) = eval_expr e env mem in (v1::l,final_mem) (* last expr to be evaluated *)
    |ASTExprs_ (e,ex) -> let (v,new_mem) = eval_expr e env mem in 
                        eval_exprs ex (v::l) env new_mem 

and eval_op op e1 e2 env mem=
 
  match op with
    Add -> 
      let (v1,m1) = eval_expr e1 env mem in 
      let (v2,m2) = eval_expr e2 env m1 in
      (InN(int_of_value (v1) + int_of_value(v2)),m2)
    | Mul ->
      let (v1,m1) = eval_expr e1 env mem in 
      let (v2,m2) = eval_expr e2 env m1 in
      (InN(int_of_value (v1) * int_of_value(v2)),m2)
    | Sub ->
      let (v1,m1) = eval_expr e1 env mem in 
      let (v2,m2) = eval_expr e2 env m1 in
      (InN(int_of_value (v1) - int_of_value(v2)),m2)
    | Div ->
      let (v1,m1) = eval_expr e1 env mem in 
      let (v2,m2) = eval_expr e2 env m1 in
      (InN(int_of_value (v1) / int_of_value(v2)),m2)
    | Or -> (
        let (v1,m1) = eval_expr e1 env mem in 
        let (v2,m2) = eval_expr e2 env m1 in
        if int_of_value (v1) + int_of_value(v2) > 0 then (*une des deux expr est vraie et vaut 1, donc >0 en tout*)
          (InN(1),m2)
        else (InN(0),m2)
    )
    | And -> (
        let (v1,m1) = eval_expr e1 env mem in 
        let (v2,m2) = eval_expr e2 env m1 in
        if int_of_value (v1) + int_of_value(v2) = 2 then (*les deux expr sont vraies donc valent 1 donc 2 en tout*)
          (InN(1),m2)
        else (InN(0),m2)
    )
    | Eq -> (
        let (v1,m1) = eval_expr e1 env mem in 
        let (v2,m2) = eval_expr e2 env m1 in
      if int_of_value (v1) = int_of_value(v2) then
        (InN(1),m2)
      else (InN(0),m2)
    )
    | Lt -> (
        let (v1,m1) = eval_expr e1 env mem in 
        let (v2,m2) = eval_expr e2 env m1 in
      if int_of_value (v1) < int_of_value(v2) then 
        (InN(1),m2)
      else (InN(0),m2)
    )
    | Nth -> (
      match eval_expr e1 env mem with
          (InA(a),m1) -> (match eval_expr e2 env m1 with 
                        (InN(i),m2) -> (value_of_ina m2 (InA(i+a)),m2)
          )
          |(InN(a),m1)-> (match eval_expr e2 env m1 with 
                          (InN(i),m2) ->(value_of_ina m2 (InA(i)),m2)
          )
      )
    


 
and eval_stat s env mem =
    match s with
    ASTEcho e -> ( match eval_expr e env mem with
                   (InN(n),m) -> print_int n; print_string "\n"; m
                   |_ -> failwith "ne s'applique que sur les entiers")
    |ASTSet(lval,e) ->  let (eval_e,emem) = eval_expr e env mem in
                        let (addr,vmem) = eval_lval lval env emem in 
                         ((InA(addr)),eval_e)::vmem
    |ASTIF(e,bk1,bk2) ->  let (eval_e,emem) = eval_expr e env mem in
                          if int_of_value (eval_e) = 1 
                          then eval_block bk1 env emem
                          else eval_block bk2 env emem
    |ASTWhile(e,bk) ->  let (eval_e,emem) = eval_expr e env mem in
                        if int_of_value (eval_e) = 1 
                            then let mem_= eval_block bk env emem in
                            eval_stat s env mem_
                            else emem
    |ASTCall(name,es)->  let (vals_list,v_mem) = eval_exprs es [] env mem in
                          let val_ = inEnv name env in 
                          (match val_ with
                          InP(bk,param,env_ ) -> eval_block bk (List.append (List.combine param vals_list) env_) v_mem
                          |InPR(name,bk,param,env_) -> 
                           eval_block bk (List.append (List.combine param vals_list) ((name,(inEnv name env))::env_)) v_mem
                           |_ -> failwith "not a procedure"
                          )

and eval_lval lval env mem = 
  
  match lval with
   ASTLvalId name -> let v = inEnv name env in 
                    (match v with
                    InA(a) -> (a,mem)
                    |InB(a,n) -> ((int_of_address a),mem) 
                    )
  | ASTLvalNth(lv,e)-> 
    let (a,mem_lv) = eval_lval lv env mem in
    let (eval_e,mem_e) = eval_expr e env mem_lv in
    (match eval_e with
      InN(i) -> ((a+i),mem_e)
    )

and eval_dec d env mem = 
    match d with
      ASTConst(name, t, e) -> let (eval_e,emem) = eval_expr e env mem in
                            ((name,(eval_e))::env,emem)
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


(* eval tout

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
    eval_list (Array.to_list arr) *)

let _ =
  let oc = open_in Sys.argv.(1) in 
  let lexbuf = Lexing.from_channel oc in
  let p = Parser.prog Lexer.token lexbuf in
  eval_prog p [] [];
  print_char '\n'
        
     