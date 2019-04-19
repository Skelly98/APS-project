assoc(X, [(X,V)|_], V).
assoc(X, [_|XS], V) :- assoc(X, XS, V).

appArg([],List,List).
appArg([arg(_,T) | A],List, [T|Result]) :- appArg(A,List,Result).

appendContext([],List,List).
appendContext([arg(X,T) | A],List, [(Z,T)|Result]) :- appendContext(A,List,Result),atom_string(X,Z).

appTypeExpr(G,[E |Tail],List,[T | Result]) :- typeExpr(G,E,T),appTypeExpr(G,Tail,List,Result).
appTypeExpr(_,[],List,List).


typeExpr(_,true,bool).
typeExpr(_,false,bool).
typeExpr(_,X,int) :- integer(X). %num
typeExpr(G,X,T) :- string(X),assoc(X,G,T). %sym
typeExpr(G,not(X),bool) :- typeExpr(G,X,bool). 
typeExpr(G,and(X,Y),bool) :- typeExpr(G,X,bool), typeExpr(G,Y,bool).
typeExpr(G,or(X,Y),bool) :- typeExpr(G,X,bool), typeExpr(G,Y,bool).
typeExpr(G,eq(X,Y),bool) :- typeExpr(G,X,int), typeExpr(G,Y,int).
typeExpr(G,lt(X,Y),bool) :- typeExpr(G,X,int), typeExpr(G,Y,int).
typeExpr(G,add(X,Y),int) :- typeExpr(G,X,int), typeExpr(G,Y,int).
typeExpr(G,sub(X,Y),int) :- typeExpr(G,X,int), typeExpr(G,Y,int).
typeExpr(G,mul(X,Y),int) :- typeExpr(G,X,int), typeExpr(G,Y,int).
typeExpr(G,div(X,Y),int) :- typeExpr(G,X,int), typeExpr(G,Y,int).
typeExpr(G,if(X,Y,Z),T) :- typeExpr(G,X,bool),typeExpr(G,Y,T),typeExpr(G,Z,T). %if
typeExpr(G,abs(args(A),E),arrow(_,Type)) :- appendContext(A,G,NewContext),typeExpr(NewContext,E,Type). %abs 
typeExpr(G,app(E,L),T) :- appTypeExpr(G,L,[],TypeRes),typeExpr(G,E,arrow(TypeRes,T)).


typeDec(G,const(X,T,E),[(Z,T) | G]) :- typeExpr(G,E,T),atom_string(X,Z). %const
typeDec(G,fun(X,T,args(A),E),[(Z,arrow(ArgsRes,T)) | G]) :- appArg(A,[],ArgsRes),appendContext(A,G,NewContext),typeExpr(NewContext,E,T),atom_string(X,Z).
typeDec(G,funRec(X,T,args(A),E),[(Z,arrow(ArgsRes,T)) | G]) :- appArg(A,[],ArgsRes),appendContext(A,G,NewContext),atom_string(X,Z),typeExpr([ (Z,arrow(ArgsRes,T)) | NewContext],E,T).

%VAR
typeDec(G,var(X,T),[(Z,T) | G ]) :- atom_string(X,Z).

%PROC
typeDec(G,proc(X,args(A),block(cmds(CS))),[(Z,arrow(ArgsRes,void))| G]) :- appArg(A,[],ArgsRes),appendContext(A,G,NewContext),atom_string(X,Z),typeCmds( NewContext,CS,void).

%PROCREC
typeDec(G,procRec(X,args(A),block(cmds(CS))),[(Z,arrow(ArgsRes,void))| G]) :- appArg(A,[],ArgsRes),appendContext(A,G,NewContext),atom_string(X,Z),typeCmds([(Z,arrow(ArgsRes,void))|NewContext],CS,void).


typeCmds(_,[],void).
typeCmds(G,[S|CS],void) :- typeStat(G,S,void), typeCmds(G,CS,void).
typeCmds(G,[D|CS],void) :- typeDec(G,D,NG), typeCmds(NG,CS,void).

%ECHO
typeStat(G,echo(X),void) :- typeExpr(G,X,int).

%SET
typeStat(G,set(ID,E),void) :- atom_string(ID,X),assoc(X,G,T),typeExpr(G,E,T).

%IF
typeStat(G,if1(E,block(cmds(Block1)),block(cmds(Block2))),void) :- typeExpr(G,E,bool),typeCmds(G,Block1,void),typeCmds(G,Block2,void).

%WHILE
typeStat(G,while(E,block(cmds(Block))),void) :- typeExpr(G,E,bool),typeCmds(G,Block,void).

%CALL
typeStat(G,call(ID,L),void) :- appTypeExpr(G,L,[],TypeRes),atom_string(ID,Z),assoc(Z,G,arrow(TypeRes,void)).

typeProg(G,prog(cmds(X)),void) :- typeCmds(G,X,void).