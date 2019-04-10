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
  | VAR
  | PROC
  | SET
  | IF_1
  | WHILE
  | CALL
  | VOID
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Ast
# 45 "parser.ml"
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
  286 (* VAR *);
  287 (* PROC *);
  288 (* SET *);
  289 (* IF_1 *);
  290 (* WHILE *);
  291 (* CALL *);
  292 (* VOID *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\002\000\002\000\002\000\004\000\004\000\004\000\
\004\000\004\000\005\000\005\000\005\000\005\000\005\000\005\000\
\008\000\008\000\008\000\008\000\010\000\010\000\011\000\009\000\
\009\000\007\000\007\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\000\000"

let yylen = "\002\000\
\004\000\003\000\001\000\003\000\003\000\002\000\003\000\004\000\
\003\000\003\000\004\000\007\000\008\000\003\000\006\000\007\000\
\001\000\001\000\001\000\005\000\001\000\003\000\003\000\001\000\
\003\000\001\000\002\000\001\000\001\000\001\000\001\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\004\000\
\004\000\004\000\006\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\044\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\028\000\031\000\000\000\000\000\029\000\
\030\000\006\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\018\000\017\000\019\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\014\000\000\000\000\000\007\000\000\000\000\000\009\000\
\000\000\010\000\001\000\005\000\004\000\000\000\000\000\011\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\008\000\027\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\040\000\000\000\000\000\
\000\000\000\000\000\000\042\000\023\000\041\000\025\000\000\000\
\000\000\002\000\022\000\000\000\000\000\000\000\032\000\033\000\
\034\000\035\000\036\000\037\000\038\000\039\000\000\000\015\000\
\000\000\020\000\012\000\000\000\043\000\016\000\013\000"

let yydgoto = "\002\000\
\004\000\014\000\063\000\015\000\016\000\065\000\066\000\070\000\
\056\000\071\000\057\000"

let yysindex = "\006\000\
\011\255\000\000\089\255\000\000\016\255\006\255\054\255\019\255\
\009\255\022\255\054\255\054\255\023\255\003\255\014\255\015\255\
\021\255\021\255\029\255\000\000\000\000\087\255\047\255\000\000\
\000\000\000\000\021\255\041\255\050\255\054\255\045\255\045\255\
\054\255\058\000\089\255\089\255\021\255\000\000\000\000\000\000\
\054\255\053\255\021\255\054\255\054\255\054\255\054\255\054\255\
\054\255\054\255\054\255\054\255\054\255\054\255\044\255\055\255\
\056\255\000\000\047\255\057\255\000\000\089\255\045\255\000\000\
\054\255\000\000\000\000\000\000\000\000\051\255\058\255\000\000\
\047\255\070\255\054\255\054\255\054\255\054\255\076\255\054\255\
\054\255\054\255\054\255\054\255\077\255\021\255\054\255\047\255\
\085\255\047\255\091\255\000\000\000\000\021\255\021\255\092\255\
\047\255\078\255\101\255\105\255\109\255\000\000\110\255\118\255\
\119\255\120\255\054\255\000\000\000\000\000\000\000\000\045\255\
\121\255\000\000\000\000\122\255\054\255\123\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\124\255\000\000\
\045\255\000\000\000\000\054\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\125\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\126\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\004\255\000\000\000\000\000\000\000\000\083\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\223\255\225\255\000\000\000\000\249\255\208\255\248\255\
\219\255\005\000\000\000"

let yytablesize = 136
let yytable = "\026\000\
\064\000\068\000\069\000\031\000\032\000\085\000\001\000\018\000\
\041\000\042\000\028\000\026\000\034\000\026\000\054\000\026\000\
\093\000\017\000\058\000\003\000\027\000\089\000\061\000\030\000\
\033\000\035\000\036\000\037\000\091\000\019\000\043\000\092\000\
\029\000\072\000\074\000\096\000\075\000\076\000\077\000\078\000\
\079\000\080\000\081\000\082\000\083\000\084\000\038\000\039\000\
\055\000\059\000\111\000\060\000\113\000\062\000\020\000\021\000\
\040\000\067\000\086\000\118\000\022\000\073\000\023\000\094\000\
\087\000\090\000\088\000\098\000\099\000\100\000\101\000\095\000\
\103\000\104\000\105\000\106\000\107\000\109\000\097\000\110\000\
\128\000\024\000\025\000\102\000\108\000\119\000\116\000\020\000\
\021\000\044\000\045\000\046\000\047\000\022\000\112\000\023\000\
\021\000\134\000\115\000\127\000\114\000\117\000\048\000\049\000\
\050\000\051\000\052\000\053\000\120\000\131\000\005\000\006\000\
\121\000\007\000\024\000\025\000\122\000\123\000\008\000\009\000\
\010\000\011\000\012\000\013\000\135\000\124\000\125\000\126\000\
\000\000\130\000\129\000\133\000\132\000\000\000\003\000\024\000"

let yycheck = "\007\000\
\032\000\035\000\036\000\011\000\012\000\054\000\001\000\002\001\
\017\000\018\000\002\001\008\001\010\001\010\001\022\000\012\001\
\065\000\002\001\027\000\009\001\002\001\059\000\030\000\002\001\
\002\001\012\001\012\001\007\001\062\000\024\001\002\001\063\000\
\024\001\041\000\043\000\073\000\044\000\045\000\046\000\047\000\
\048\000\049\000\050\000\051\000\052\000\053\000\026\001\027\001\
\002\001\009\001\088\000\002\001\090\000\009\001\001\001\002\001\
\036\001\000\000\015\001\097\000\007\001\009\001\009\001\013\001\
\010\001\009\001\011\001\075\000\076\000\077\000\078\000\014\001\
\080\000\081\000\082\000\083\000\084\000\086\000\009\001\087\000\
\112\000\028\001\029\001\008\001\008\001\008\001\095\000\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\010\001\009\001\
\014\001\129\000\094\000\107\000\010\001\010\001\016\001\017\001\
\018\001\019\001\020\001\021\001\008\001\117\000\022\001\023\001\
\008\001\025\001\028\001\029\001\008\001\008\001\030\001\031\001\
\032\001\033\001\034\001\035\001\132\000\008\001\008\001\008\001\
\255\255\008\001\010\001\008\001\010\001\255\255\010\001\010\001"

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
  VAR\000\
  PROC\000\
  SET\000\
  IF_1\000\
  WHILE\000\
  CALL\000\
  VOID\000\
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
# 31 "parser.mly"
                              ( ASTProg(_2))
# 259 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cmds) in
    Obj.repr(
# 33 "parser.mly"
                              ( ASTBlock(_2))
# 266 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 36 "parser.mly"
         (ASTStat (_1))
# 273 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 37 "parser.mly"
                 (ASTDecCmds(_1,_3))
# 281 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 38 "parser.mly"
                  (ASTStatCmds(_1,_3))
# 289 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 41 "parser.mly"
              ( ASTEcho(_2) )
# 296 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 42 "parser.mly"
                     ( ASTSet(_2,_3) )
# 304 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 43 "parser.mly"
                           ( ASTIF(_2,_3,_4) )
# 313 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 44 "parser.mly"
                      ( ASTWhile(_2,_3) )
# 321 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 45 "parser.mly"
                      ( ASTCall(_2,_3) )
# 329 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'type_) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 48 "parser.mly"
                           ( ASTConst(_2,_3,_4) )
# 338 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'type_) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 49 "parser.mly"
                                               ( ASTFun(_2,_3,_5,_7))
# 348 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'type_) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 50 "parser.mly"
                                                   ( ASTFunRec(_3,_4,_6,_8))
# 358 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'type_) in
    Obj.repr(
# 51 "parser.mly"
                      ( ASTVar(_2,_3) )
# 366 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 52 "parser.mly"
                                            ( ASTProc(_2,_4,_6) )
# 375 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 53 "parser.mly"
                                                ( ASTProc(_3,_5,_7) )
# 384 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
         (ASTInt)
# 390 "parser.ml"
               : 'type_))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
           (ASTBool)
# 396 "parser.ml"
               : 'type_))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
           ( ASTVoid )
# 402 "parser.ml"
               : 'type_))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'type_) in
    Obj.repr(
# 60 "parser.mly"
                                  ( ASTArrow(_2,_4) )
# 410 "parser.ml"
               : 'type_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'type_) in
    Obj.repr(
# 63 "parser.mly"
          ( ASTType(_1) )
# 417 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'type_) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 64 "parser.mly"
                      ( ASTTypes(_1,_3))
# 425 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'type_) in
    Obj.repr(
# 67 "parser.mly"
                      (ASTColon(_1,_3))
# 433 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 70 "parser.mly"
        ( ASTArg(_1) )
# 440 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 71 "parser.mly"
                     ( ASTArgs(_1,_3) )
# 448 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
         (ASTExpr(_1))
# 455 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 75 "parser.mly"
                (ASTExprs_(_1,_2))
# 463 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 78 "parser.mly"
        ( ASTNum(_1) )
# 470 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
           ( ASTTrue )
# 476 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
            ( ASTFalse )
# 482 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 81 "parser.mly"
            ( ASTId(_1) )
# 489 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 82 "parser.mly"
                               ( ASTPrim(Ast.Add, _3, _4) )
# 497 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 83 "parser.mly"
                                ( ASTPrim(Ast.Sub, _3, _4) )
# 505 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 84 "parser.mly"
                                ( ASTPrim(Ast.Mul, _3, _4) )
# 513 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 85 "parser.mly"
                              ( ASTPrim(Ast.Div, _3, _4) )
# 521 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 86 "parser.mly"
                             ( ASTPrim(Ast.Or, _3, _4) )
# 529 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 87 "parser.mly"
                              ( ASTPrim(Ast.And, _3, _4) )
# 537 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 88 "parser.mly"
                             ( ASTPrim(Ast.Eq, _3, _4) )
# 545 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 89 "parser.mly"
                             ( ASTPrim(Ast.Lt, _3, _4) )
# 553 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 90 "parser.mly"
                         ( ASTUnary(Ast.Not, _3) )
# 560 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "parser.mly"
                                ( ASTAbs(_2,_4) )
# 568 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 92 "parser.mly"
                           ( ASTApp(_2,_3) )
# 576 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 93 "parser.mly"
                                   ( ASTIf(_3,_4,_5) )
# 585 "parser.ml"
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
