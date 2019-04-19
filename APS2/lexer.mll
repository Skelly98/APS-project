{
  open Parser (* The type token is defined in parser.mli *)
  exception Eof
}

rule token = parse
  [' ' '\t' '\n'] { token lexbuf } (* skip blanks *)
  | ['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
  | "bool" { BOOL }
  | "int"   { INT }
  | "add" { PLUS }
  | "sub" { MINUS }
  | "mul" { TIMES }
  | "div" { DIV }
  | "true"  { TRUE }
  | "false" { FALSE }
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }
  | "eq" { EQ }
  | "lt" { LT }
  | "if" { IF }
  | '['   { LSQUARE }
  | ']'   { RSQUARE }
  | ';'   { SC }
  | ','   { COMMA }
  | '*'   { STAR }
  | ':'   { COLON }
  | "->"   { ARROW }
  | "CONST"   { CONST }
  | "FUN"   { FUN }
  | "REC"   { REC }
  | "ECHO"   { ECHO }
  | '(' { LPAR }
  | ')' { RPAR }
  | "VAR" { VAR }
  | "PROC" { PROC }
  | "SET" { SET }
  | "IF" { IF_1 }
  | "WHILE" { WHILE }
  | "CALL" { CALL }
  | "void" { VOID }
  | "len"  { LEN }
  | "alloc" { ALLOC }
  | "nth"   { NTH }
  | "vec"   { VEC }
  | ['a'-'z' 'A'-'Z'] (['a'-'z' 'A'-'Z' '0'-'9']*) as x {IDENT(x)}
  | eof { EOF }