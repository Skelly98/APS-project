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

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.prog
