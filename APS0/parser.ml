type token =
  | NUM of (int)
  | IDENT of (string)
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | LPAR
  | RPAR
  | LSQUARE
  | RSQUARE
  | COMMA
  | SC
  | STAR
  | ARROW
  | COLON
  | NOT
  | OR
  | AND
  | EQ
  | LT
  | IF
  | CONST
  | FUN
  | REC
  | ECHO
  | BOOL
  | INT
  | TRUE
  | FALSE
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Ast
# 38 "parser.ml"
let yytransl_const = [|
  259 (* PLUS *);
  260 (* MINUS *);
  261 (* TIMES *);
  262 (* DIV *);
  263 (* LPAR *);
  264 (* RPAR *);
  265 (* LSQUARE *);
  266 (* RSQUARE *);
  267 (* COMMA *);
  268 (* SC *);
  269 (* STAR *);
  270 (* ARROW *);
  271 (* COLON *);
  272 (* NOT *);
  273 (* OR *);
  274 (* AND *);
  275 (* EQ *);
  276 (* LT *);
  277 (* IF *);
  278 (* CONST *);
  279 (* FUN *);
  280 (* REC *);
  281 (* ECHO *);
  282 (* BOOL *);
  283 (* INT *);
  284 (* TRUE *);
  285 (* FALSE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\004\000\004\000\004\000\
\006\000\006\000\006\000\008\000\008\000\009\000\007\000\007\000\
\010\000\010\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\000\000"

let yylen = "\002\000\
\004\000\001\000\003\000\003\000\002\000\004\000\007\000\008\000\
\001\000\001\000\005\000\001\000\003\000\003\000\001\000\003\000\
\001\000\002\000\001\000\001\000\001\000\001\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\004\000\004\000\
\004\000\006\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\035\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\019\000\022\000\000\000\
\000\000\020\000\021\000\005\000\000\000\000\000\000\000\000\000\
\010\000\009\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\004\000\003\000\000\000\000\000\
\006\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\031\000\000\000\000\000\000\000\000\000\000\000\
\018\000\033\000\014\000\032\000\016\000\013\000\000\000\000\000\
\000\000\023\000\024\000\025\000\026\000\027\000\028\000\029\000\
\030\000\000\000\011\000\007\000\000\000\034\000\008\000"

let yydgoto = "\002\000\
\004\000\008\000\009\000\010\000\062\000\047\000\042\000\048\000\
\043\000\063\000"

let yysindex = "\004\000\
\248\254\000\000\016\255\000\000\006\255\011\255\005\255\009\255\
\003\255\004\255\010\255\010\255\038\255\000\000\000\000\081\255\
\040\255\000\000\000\000\000\000\043\000\016\255\016\255\010\255\
\000\000\000\000\005\255\035\255\010\255\005\255\005\255\005\255\
\005\255\005\255\005\255\005\255\005\255\005\255\005\255\005\255\
\034\255\045\255\046\255\000\000\000\000\000\000\048\255\049\255\
\000\000\040\255\050\255\005\255\005\255\005\255\005\255\054\255\
\005\255\005\255\005\255\005\255\005\255\005\255\056\255\010\255\
\005\255\040\255\010\255\010\255\055\255\040\255\058\255\059\255\
\060\255\061\255\000\000\062\255\063\255\064\255\066\255\005\255\
\000\000\000\000\000\000\000\000\000\000\000\000\067\255\005\255\
\068\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\069\255\000\000\000\000\005\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\070\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\079\255\000\000\000\000\000\000\065\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\083\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\244\255\000\000\000\000\249\255\248\255\208\255\009\000\
\000\000\030\000"

let yytablesize = 110
let yytable = "\020\000\
\003\000\069\000\027\000\028\000\001\000\014\000\015\000\011\000\
\040\000\045\000\046\000\016\000\012\000\017\000\022\000\023\000\
\024\000\085\000\021\000\049\000\051\000\089\000\052\000\053\000\
\054\000\055\000\056\000\057\000\058\000\059\000\060\000\061\000\
\018\000\019\000\013\000\025\000\026\000\005\000\006\000\029\000\
\007\000\041\000\044\000\050\000\071\000\072\000\073\000\074\000\
\064\000\076\000\077\000\078\000\079\000\080\000\065\000\083\000\
\066\000\084\000\070\000\087\000\067\000\075\000\068\000\082\000\
\088\000\090\000\091\000\092\000\093\000\094\000\095\000\096\000\
\098\000\097\000\099\000\086\000\102\000\101\000\012\000\002\000\
\100\000\014\000\015\000\030\000\031\000\032\000\033\000\016\000\
\015\000\017\000\017\000\081\000\000\000\103\000\000\000\000\000\
\034\000\035\000\036\000\037\000\038\000\039\000\000\000\000\000\
\000\000\000\000\000\000\000\000\018\000\019\000"

let yycheck = "\007\000\
\009\001\050\000\011\000\012\000\001\000\001\001\002\001\002\001\
\016\000\022\000\023\000\007\001\002\001\009\001\012\001\012\001\
\007\001\066\000\010\001\027\000\029\000\070\000\030\000\031\000\
\032\000\033\000\034\000\035\000\036\000\037\000\038\000\039\000\
\028\001\029\001\024\001\026\001\027\001\022\001\023\001\002\001\
\025\001\002\001\000\000\009\001\052\000\053\000\054\000\055\000\
\015\001\057\000\058\000\059\000\060\000\061\000\010\001\064\000\
\011\001\065\000\009\001\068\000\013\001\008\001\014\001\008\001\
\010\001\008\001\008\001\008\001\008\001\008\001\008\001\008\001\
\080\000\008\001\008\001\067\000\008\001\010\001\014\001\010\001\
\088\000\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\010\001\009\001\008\001\062\000\255\255\101\000\255\255\255\255\
\016\001\017\001\018\001\019\001\020\001\021\001\255\255\255\255\
\255\255\255\255\255\255\255\255\028\001\029\001"

let yynames_const = "\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  LPAR\000\
  RPAR\000\
  LSQUARE\000\
  RSQUARE\000\
  COMMA\000\
  SC\000\
  STAR\000\
  ARROW\000\
  COLON\000\
  NOT\000\
  OR\000\
  AND\000\
  EQ\000\
  LT\000\
  IF\000\
  CONST\000\
  FUN\000\
  REC\000\
  ECHO\000\
  BOOL\000\
  INT\000\
  TRUE\000\
  FALSE\000\
  EOF\000\
  "

let yynames_block = "\
  NUM\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'cmds) in
    Obj.repr(
# 29 "parser.mly"
                              ( ASTProg(_2))
# 218 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 32 "parser.mly"
         (ASTStat (_1))
# 225 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 33 "parser.mly"
                 (ASTDecCmds(_1,_3))
# 233 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 34 "parser.mly"
                  (ASTStatCmds(_1,_3))
# 241 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 37 "parser.mly"
              ( ASTEcho(_2) )
# 248 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'type_) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 40 "parser.mly"
                           ( ASTConst(_2,_3,_4) )
# 257 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'type_) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 41 "parser.mly"
                                               ( ASTFun(_2,_3,_5,_7))
# 267 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'type_) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 42 "parser.mly"
                                                   ( ASTFunRec(_3,_4,_6,_8))
# 277 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "parser.mly"
         (ASTInt)
# 283 "parser.ml"
               : 'type_))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
           (ASTBool)
# 289 "parser.ml"
               : 'type_))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'type_) in
    Obj.repr(
# 47 "parser.mly"
                                  ( ASTArrow(_2,_4) )
# 297 "parser.ml"
               : 'type_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'type_) in
    Obj.repr(
# 50 "parser.mly"
          ( ASTType(_1) )
# 304 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'type_) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 51 "parser.mly"
                      ( ASTTypes(_1,_3))
# 312 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'type_) in
    Obj.repr(
# 54 "parser.mly"
                      (ASTColon(_1,_3))
# 320 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 57 "parser.mly"
        ( ASTArg(_1) )
# 327 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 58 "parser.mly"
                     ( ASTArgs(_1,_3) )
# 335 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 61 "parser.mly"
         (ASTExpr(_1))
# 342 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 62 "parser.mly"
                (ASTExprs_(_1,_2))
# 350 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 65 "parser.mly"
        ( ASTNum(_1) )
# 357 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "parser.mly"
           ( ASTTrue )
# 363 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "parser.mly"
            ( ASTFalse )
# 369 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 68 "parser.mly"
            ( ASTId(_1) )
# 376 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
                               ( ASTPrim(Ast.Add, _3, _4) )
# 384 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 70 "parser.mly"
                                ( ASTPrim(Ast.Sub, _3, _4) )
# 392 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                                ( ASTPrim(Ast.Mul, _3, _4) )
# 400 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                              ( ASTPrim(Ast.Div, _3, _4) )
# 408 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
                             ( ASTPrim(Ast.Or, _3, _4) )
# 416 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
                              ( ASTPrim(Ast.And, _3, _4) )
# 424 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                             ( ASTPrim(Ast.Eq, _3, _4) )
# 432 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
                             ( ASTPrim(Ast.Lt, _3, _4) )
# 440 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 77 "parser.mly"
                         ( ASTUnary(Ast.Not, _3) )
# 447 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 78 "parser.mly"
                                ( ASTAbs(_2,_4) )
# 455 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 79 "parser.mly"
                           ( ASTApp(_2,_3) )
# 463 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 80 "parser.mly"
                                   ( ASTIf(_3,_4,_5) )
# 472 "parser.ml"
               : 'expr))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.prog)
