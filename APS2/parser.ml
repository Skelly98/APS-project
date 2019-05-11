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
  | LEN
  | ALLOC
  | NTH
  | VEC
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Ast
# 49 "parser.ml"
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
  293 (* LEN *);
  294 (* ALLOC *);
  295 (* NTH *);
  296 (* VEC *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\002\000\002\000\002\000\004\000\004\000\004\000\
\004\000\004\000\005\000\005\000\005\000\005\000\005\000\005\000\
\009\000\009\000\009\000\009\000\009\000\011\000\011\000\012\000\
\010\000\010\000\008\000\008\000\007\000\007\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\000\000"

let yylen = "\002\000\
\004\000\003\000\001\000\003\000\003\000\002\000\003\000\004\000\
\003\000\003\000\004\000\007\000\008\000\003\000\006\000\007\000\
\001\000\001\000\001\000\005\000\004\000\001\000\003\000\003\000\
\001\000\003\000\001\000\002\000\001\000\005\000\001\000\001\000\
\001\000\001\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\004\000\004\000\004\000\006\000\004\000\004\000\
\005\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\050\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\031\000\034\000\000\000\000\000\032\000\
\033\000\006\000\000\000\000\000\000\000\029\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\018\000\
\017\000\019\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\014\000\000\000\
\000\000\000\000\007\000\000\000\000\000\009\000\000\000\010\000\
\001\000\005\000\004\000\000\000\000\000\000\000\011\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\008\000\028\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\043\000\000\000\000\000\000\000\000\000\000\000\048\000\
\047\000\000\000\045\000\024\000\044\000\026\000\000\000\000\000\
\000\000\002\000\021\000\023\000\000\000\000\000\000\000\035\000\
\036\000\037\000\038\000\039\000\040\000\041\000\042\000\000\000\
\049\000\015\000\000\000\030\000\020\000\012\000\000\000\046\000\
\016\000\013\000"

let yydgoto = "\002\000\
\004\000\014\000\069\000\015\000\016\000\071\000\032\000\072\000\
\077\000\061\000\078\000\062\000"

let yysindex = "\002\000\
\001\255\000\000\124\255\000\000\009\255\255\254\123\255\011\255\
\006\255\007\255\123\255\123\255\015\255\008\255\014\255\016\255\
\062\255\062\255\018\255\000\000\000\000\100\255\030\255\000\000\
\000\000\000\000\062\255\024\255\052\255\000\000\017\255\123\255\
\050\255\050\255\123\255\060\000\124\255\124\255\031\255\000\000\
\000\000\000\000\123\255\053\255\062\255\123\255\123\255\123\255\
\123\255\123\255\123\255\123\255\123\255\123\255\123\255\123\255\
\123\255\123\255\123\255\046\255\056\255\057\255\000\000\030\255\
\054\255\007\255\000\000\124\255\050\255\000\000\123\255\000\000\
\000\000\000\000\000\000\062\255\059\255\060\255\000\000\030\255\
\061\255\123\255\123\255\123\255\123\255\065\255\123\255\123\255\
\123\255\123\255\123\255\071\255\078\255\123\255\083\255\062\255\
\123\255\030\255\082\255\030\255\123\255\087\255\000\000\000\000\
\085\255\062\255\062\255\089\255\030\255\092\255\102\255\103\255\
\105\255\000\000\106\255\114\255\115\255\118\255\123\255\000\000\
\000\000\125\255\000\000\000\000\000\000\000\000\050\255\098\255\
\126\255\000\000\000\000\000\000\127\255\123\255\121\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\128\255\
\000\000\000\000\050\255\000\000\000\000\000\000\123\255\000\000\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\130\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\131\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\019\255\000\000\
\000\000\000\000\000\000\000\000\129\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\240\255\224\255\000\000\000\000\249\255\076\000\209\255\
\245\255\211\255\039\000\000\000"

let yytablesize = 159
let yytable = "\026\000\
\018\000\070\000\001\000\033\000\034\000\043\000\044\000\028\000\
\030\000\003\000\017\000\095\000\027\000\031\000\059\000\063\000\
\035\000\036\000\099\000\045\000\074\000\075\000\019\000\104\000\
\067\000\037\000\027\000\038\000\027\000\029\000\027\000\060\000\
\064\000\081\000\108\000\079\000\103\000\039\000\082\000\083\000\
\084\000\085\000\086\000\087\000\088\000\089\000\090\000\091\000\
\092\000\093\000\094\000\102\000\126\000\065\000\128\000\066\000\
\040\000\041\000\068\000\073\000\096\000\080\000\100\000\135\000\
\105\000\097\000\042\000\098\000\039\000\109\000\076\000\106\000\
\114\000\107\000\110\000\111\000\112\000\113\000\120\000\115\000\
\116\000\117\000\118\000\119\000\124\000\121\000\122\000\040\000\
\041\000\125\000\123\000\127\000\131\000\129\000\146\000\133\000\
\130\000\042\000\134\000\136\000\020\000\021\000\046\000\047\000\
\048\000\049\000\022\000\147\000\023\000\137\000\138\000\144\000\
\139\000\140\000\153\000\050\000\051\000\052\000\053\000\054\000\
\055\000\141\000\142\000\020\000\021\000\143\000\150\000\024\000\
\025\000\022\000\151\000\023\000\145\000\148\000\149\000\152\000\
\056\000\057\000\058\000\003\000\025\000\101\000\022\000\154\000\
\132\000\005\000\006\000\000\000\007\000\000\000\024\000\025\000\
\000\000\008\000\009\000\010\000\011\000\012\000\013\000"

let yycheck = "\007\000\
\002\001\034\000\001\000\011\000\012\000\017\000\018\000\002\001\
\002\001\009\001\002\001\059\000\002\001\007\001\022\000\027\000\
\002\001\010\001\064\000\002\001\037\000\038\000\024\001\071\000\
\032\000\012\001\008\001\012\001\010\001\024\001\012\001\002\001\
\009\001\045\000\080\000\043\000\069\000\007\001\046\000\047\000\
\048\000\049\000\050\000\051\000\052\000\053\000\054\000\055\000\
\056\000\057\000\058\000\068\000\098\000\002\001\100\000\039\001\
\026\001\027\001\009\001\000\000\015\001\009\001\009\001\109\000\
\076\000\010\001\036\001\011\001\007\001\009\001\040\001\013\001\
\008\001\014\001\082\000\083\000\084\000\085\000\008\001\087\000\
\088\000\089\000\090\000\091\000\096\000\008\001\094\000\026\001\
\027\001\097\000\008\001\010\001\008\001\101\000\127\000\107\000\
\010\001\036\001\010\001\008\001\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\010\001\009\001\008\001\008\001\119\000\
\008\001\008\001\147\000\016\001\017\001\018\001\019\001\020\001\
\021\001\008\001\008\001\001\001\002\001\008\001\134\000\028\001\
\029\001\007\001\010\001\009\001\008\001\008\001\008\001\008\001\
\037\001\038\001\039\001\010\001\010\001\066\000\014\001\151\000\
\106\000\022\001\023\001\255\255\025\001\255\255\028\001\029\001\
\255\255\030\001\031\001\032\001\033\001\034\001\035\001"

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
  LEN\000\
  ALLOC\000\
  NTH\000\
  VEC\000\
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
# 32 "parser.mly"
                              ( ASTProg(_2))
# 288 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cmds) in
    Obj.repr(
# 34 "parser.mly"
                              ( ASTBlock(_2))
# 295 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 37 "parser.mly"
         (ASTStat (_1))
# 302 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 38 "parser.mly"
                 (ASTDecCmds(_1,_3))
# 310 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 39 "parser.mly"
                  (ASTStatCmds(_1,_3))
# 318 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 42 "parser.mly"
              ( ASTEcho(_2) )
# 325 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'lval) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 43 "parser.mly"
                    ( ASTSet(_2,_3) )
# 333 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 44 "parser.mly"
                           ( ASTIF(_2,_3,_4) )
# 342 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 45 "parser.mly"
                      ( ASTWhile(_2,_3) )
# 350 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 46 "parser.mly"
                      ( ASTCall(_2,_3) )
# 358 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'type_) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 49 "parser.mly"
                           ( ASTConst(_2,_3,_4) )
# 367 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'type_) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 50 "parser.mly"
                                               ( ASTFun(_2,_3,_5,_7))
# 377 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'type_) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 51 "parser.mly"
                                                   ( ASTFunRec(_3,_4,_6,_8))
# 387 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'type_) in
    Obj.repr(
# 52 "parser.mly"
                      ( ASTVar(_2,_3) )
# 395 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 53 "parser.mly"
                                            ( ASTProc(_2,_4,_6) )
# 404 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 54 "parser.mly"
                                                ( ASTProcRec(_3,_5,_7) )
# 413 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
         (ASTInt)
# 419 "parser.ml"
               : 'type_))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
           (ASTBool)
# 425 "parser.ml"
               : 'type_))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
           ( ASTVoid )
# 431 "parser.ml"
               : 'type_))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'type_) in
    Obj.repr(
# 61 "parser.mly"
                                  ( ASTArrow(_2,_4) )
# 439 "parser.ml"
               : 'type_))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'type_) in
    Obj.repr(
# 62 "parser.mly"
                          ( ASTVec(_3) )
# 446 "parser.ml"
               : 'type_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'type_) in
    Obj.repr(
# 65 "parser.mly"
          ( ASTType(_1) )
# 453 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'type_) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 66 "parser.mly"
                      ( ASTTypes(_1,_3))
# 461 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'type_) in
    Obj.repr(
# 69 "parser.mly"
                      (ASTColon(_1,_3))
# 469 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 72 "parser.mly"
        ( ASTArg(_1) )
# 476 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 73 "parser.mly"
                     ( ASTArgs(_1,_3) )
# 484 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
         (ASTExpr(_1))
# 491 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 77 "parser.mly"
                (ASTExprs_(_1,_2))
# 499 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 80 "parser.mly"
          (ASTLvalId(_1))
# 506 "parser.ml"
               : 'lval))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'lval) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 81 "parser.mly"
                             ( ASTLvalNth(_3,_4))
# 514 "parser.ml"
               : 'lval))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 84 "parser.mly"
        ( ASTNum(_1) )
# 521 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
           ( ASTTrue )
# 527 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
            ( ASTFalse )
# 533 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 87 "parser.mly"
            ( ASTId(_1) )
# 540 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 88 "parser.mly"
                               ( ASTPrim(Ast.Add, _3, _4) )
# 548 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 89 "parser.mly"
                                ( ASTPrim(Ast.Sub, _3, _4) )
# 556 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 90 "parser.mly"
                                ( ASTPrim(Ast.Mul, _3, _4) )
# 564 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 91 "parser.mly"
                              ( ASTPrim(Ast.Div, _3, _4) )
# 572 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 92 "parser.mly"
                             ( ASTPrim(Ast.Or, _3, _4) )
# 580 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 93 "parser.mly"
                              ( ASTPrim(Ast.And, _3, _4) )
# 588 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 94 "parser.mly"
                             ( ASTPrim(Ast.Eq, _3, _4) )
# 596 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 95 "parser.mly"
                             ( ASTPrim(Ast.Lt, _3, _4) )
# 604 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 96 "parser.mly"
                         ( ASTUnary(Ast.Not, _3) )
# 611 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "parser.mly"
                                ( ASTAbs(_2,_4) )
# 619 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 98 "parser.mly"
                           ( ASTApp(_2,_3) )
# 627 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
                                   ( ASTIf(_3,_4,_5) )
# 636 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
                           ( ASTUnary(Ast.Alloc,_3) )
# 643 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                          (ASTUnary(Ast.Len,_3))
# 650 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                              ( ASTPrim(Ast.Nth,_3,_4))
# 658 "parser.ml"
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
