assoc(X, [(X,V)|_], V).
assoc(X, [_|XS], V) :- assoc(X, XS, V).

appArg([],List,List).
appArg([arg(_,T) | A],List, [T|Result]) :- appArg(A,List,Result).

appendContext([],List,List).
appendContext([arg(X,T) | A],List, [(Z,T)|Result]) :- appendContext(A,List,Result),atom_string(X,Z).

appEtoile(etoile(Head,Tail),List,[Head | Result]) :- appEtoile(Tail,List,Result).
appEtoile(T,List,[T | List]).

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
typeExpr(G,abs(args(A),E),arrow(Types,Type)) :- appArg(A,[],ArgsRes),appendContext(A,G,NewContext),typeExpr(NewContext,E,Type),compare(=,ArgsRes,Types). %abs 
typeExpr(G,app(E,L),T) :- appTypeExpr(G,L,[],TypeRes),typeExpr(G,E,arrow(TypeRes,T)).


typeDec(G,const(X,T,E),[(Z,T) | G]) :- typeExpr(G,E,T),atom_string(X,Z). %const
typeDec(G,fun(X,T,args(A),E),[(Z,arrow(ArgsRes,T)) | G]) :- appArg(A,[],ArgsRes),appendContext(A,G,NewContext),typeExpr(NewContext,E,T),atom_string(X,Z).
typeDec(G,funRec(X,T,args(A),E),[(Z,arrow(ArgsRes,T)) | G]) :- appArg(A,[],ArgsRes),appendContext(A,G,NewContext),atom_string(X,Z),typeExpr([ (Z,arrow(ArgsRes,T)) | NewContext],E,T).

typeCmds(_,[],void).
typeCmds(G,[S|CS],void) :- typeStat(G,S,void), typeCmds(G,CS,void).
typeCmds(G,[D|CS],void) :- typeDec(G,D,NG), typeCmds(NG,CS,void).


typeStat(G,echo(X),void) :- typeExpr(G,X,int).

typeProg(G,prog(cmds(X)),void) :- typeCmds(G,X,void).