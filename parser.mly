%{
open Ast
%}
%token <int> NUM
%token <string> IDENT
%token PLUS MINUS TIMES DIV
%token LPAR RPAR LSQUARE RSQUARE
%token COMMA SC STAR ARROW COLON
%token NOT OR AND EQ LT IF
%token CONST FUN REC ECHO
%token BOOL INT TRUE FALSE

%token EOF
%start prog /* the entry point */
%type <Ast.prog> prog
%type <Ast.cmds>
%type <Ast.stat> 
%type <Ast.dec> 
%type <Ast.type_>
%type <Ast.types>
%type <Ast.arg>
%type <Ast.args>
%type <Ast.exprs> 
%type <Ast.expr> 

%%

  prog:
    LSQUARE cmds RSQUARE  EOF { ASTProg($2)}

  cmds:
    stat {ASTStat ($1)}
    |dec SC cmds {ASTDecCmds($1,$3)}
    |stat SC cmds {ASTStatCmds($1,$3)}

  stat:
    ECHO expr { ASTEcho($2) }

  dec:
    CONST IDENT type_ expr { ASTConst($2,$3,$4) }
    |FUN IDENT type_ LSQUARE args RSQUARE expr { ASTFun($2,$3,$5,$7)}
    |FUN REC IDENT type_ LSQUARE args RSQUARE expr { ASTFunRec($3,$4,$6,$8)} 

  type_ :
     INT {ASTInt}
    | BOOL {ASTBool}
    | LPAR types ARROW type_ RPAR { ASTArrow($2,$4) }

  types :
    type_ { ASTType($1) } 
    |type_ STAR types { ASTTypes($1,$3)}

  arg :
    IDENT COLON type_ {ASTColon($1,$3)}

  args:
    arg { ASTArg($1) }
    | arg COMMA args { ASTArgs($1,$3) }

  exprs:
    expr {ASTExpr($1)}
    |expr exprs {ASTExprs_($1,$2)}

  expr:
    NUM { ASTNum($1) }
    | TRUE { ASTTrue }
    | FALSE { ASTFalse }
    | IDENT { ASTId($1) }
    | LPAR PLUS expr expr RPAR { ASTPrim(Ast.Add, $3, $4) }
    | LPAR MINUS expr expr RPAR { ASTPrim(Ast.Sub, $3, $4) }
    | LPAR TIMES expr expr RPAR { ASTPrim(Ast.Mul, $3, $4) }
    | LPAR DIV expr expr RPAR { ASTPrim(Ast.Div, $3, $4) }
    | LPAR OR expr expr RPAR { ASTPrim(Ast.Or, $3, $4) }
    | LPAR AND expr expr RPAR { ASTPrim(Ast.And, $3, $4) }
    | LPAR EQ expr expr RPAR { ASTPrim(Ast.Eq, $3, $4) }
    | LPAR LT expr expr RPAR { ASTPrim(Ast.Lt, $3, $4) }
    | LPAR NOT expr RPAR { ASTUnary(Ast.Not, $3) }
    | LSQUARE args RSQUARE expr { ASTAbs($2,$4) }
    | LPAR expr exprs RPAR { ASTApp($2,$3) }    
    | LPAR IF expr expr expr  RPAR { ASTIf($3,$4,$5) }

  ;
