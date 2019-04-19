open Ast

let rec print_type type_ =
  match type_ with
    ASTInt ->    Printf.printf"int";
    |ASTBool ->  Printf.printf"bool";
    |ASTArrow(ts,t) ->
      Printf.printf"arrow(";
      print_types ts;
      Printf.printf",";
      print_type t;
      Printf.printf")"
    
and print_types l =
  match l with
  ASTType(t) -> 
    Printf.printf"[";
    print_type t;
    Printf.printf"]"
  |ASTTypes(t,ts) -> (
      Printf.printf"[";
      print_type t;
      Printf.printf",";
      print_types_aux ts;
      Printf.printf"]"
  )

and print_types_aux tips =
  match tips with
    ASTType(t) ->
      print_type t
    |ASTTypes(t,ts) ->
      print_type t;
      Printf.printf ",";
      print_types_aux ts


let print_arg a =
  match a with
    ASTColon(name,t) -> 
      Printf.printf"arg(";
      Printf.printf"%s," name;
      print_type t;
      Printf.printf")"

let rec print_args l = 
  match l with
    ASTArg(a) -> print_arg a
    |ASTArgs(a,al) -> 
      print_arg a;
      Printf.printf", "; 
      print_args al



let rec print_expr e =
  match e with
    ASTNum n -> Printf.printf"%d" n
    | ASTId x -> Printf.printf"\"%s\"" x
    | ASTPrim(op, e1, e2) -> (
      Printf.printf"%s" (string_of_op op);
      Printf.printf"(";
      print_expr e1;
      Printf.printf",";
      print_expr e2;
      Printf.printf")"
    )
    | ASTUnary(op,e) -> (
      Printf.printf"%s" (string_of_op op);
      Printf.printf"(";
      print_expr e;
      Printf.printf")"
    )
    |ASTTrue ->  Printf.printf"true"
    |ASTFalse ->  Printf.printf"false" 
    |ASTIf(e1,e2,e3) ->  (
      Printf.printf"if(";
      print_expr e1;
      Printf.printf" , ";
      print_expr e2;
      Printf.printf" , ";
      print_expr e3;
      Printf.printf") "
    )
    |ASTAbs(args,e) -> (
      Printf.printf"abs(";
      Printf.printf"args([";
      print_args args;
      Printf.printf"])";
      Printf.printf", ";
      print_expr e;
      Printf.printf")"
    )
    |ASTApp(e,exs) ->(
      Printf.printf"app(";
      print_expr e;
      Printf.printf",[";
      print_exprs exs;
      Printf.printf"]";
      Printf.printf")"
    )
    
  and print_exprs e = 
    match e with 
    ASTExpr e -> print_expr e 
    |ASTExprs_(e,l) -> (
      (print_expr e);
      Printf.printf",";
      print_exprs l
    )

  let print_dec d =
    match d with
      ASTConst(name, t, e) ->  (
        Printf.printf"const(";
        Printf.printf"%s," name;
        print_type t;
        Printf.printf",";
        print_expr e;
        Printf.printf")"
      )
      |ASTFun(name, t, a, e) -> 
        Printf.printf"fun(";
        Printf.printf"%s," name;
        print_type t;
        Printf.printf",";
        Printf.printf"args([";
        print_args a;
        Printf.printf"])";
        Printf.printf",";
        print_expr e;
        Printf.printf")"
      |ASTFunRec(name, t, a, e) ->
        Printf.printf"funRec(";
        Printf.printf"%s," name;
        print_type t;
        Printf.printf",";
        Printf.printf"args([";
        print_args a;
        Printf.printf"])";
        Printf.printf",";
        print_expr e;
        Printf.printf")"

  let print_stat s =
    match s with
      ASTEcho e -> (
        Printf.printf("echo(");
        print_expr e;
        Printf.printf(")")
      )

  let rec print_cmds c =
    match c with 
      ASTStat s -> (
        print_stat s
      )
      |ASTDecCmds(d,cmds) -> (
        print_dec d;
        Printf.printf",";
        print_cmds cmds
      )
      |ASTStatCmds(s,cmds) -> (
        print_stat s;
        Printf.printf",";
        print_cmds cmds
      )

  let print_prog p = 
    match p with 
      ASTProg cmds -> (
        Printf.printf("prog(cmds([");
        print_cmds cmds;
        Printf.printf("]))")
      )

let rec print_list = function 
[] -> exit 0
| e::l ->   let oc = open_in ("exemple/"^e) in 
            let lexbuf = Lexing.from_channel oc in
            let p = Parser.prog Lexer.token lexbuf in
              print_string e;
              print_char '\n';
              print_prog p;
              print_char '\n';
              print_list l

let _ = 
  let arr = Sys.readdir "exemple" in
    print_list (Array.to_list arr)
  


        
     