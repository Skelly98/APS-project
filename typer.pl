assoc(X, [(X,V)|_], V).
assoc(X, [_|XS], V) :- assoc(X, XS, V).

app([],_).
app([arg(_,T) | A], [T|L]) :- app(A,L).


typeExpr(_,true,bool).
typeExpr(_,false,bool).
typeExpr(_,X,int) :- integer(X). %num
typeExpr(G,X,T) :- string(X), assoc(X,G,T). %sym
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
%typeExpr(G,args(A),E, [T | TS]) :- add_context(G,A,NG) , typeExpr(NG,E,TS).


typeDec(G,const(X,T,E),[(X,T) | G]) :- typeExpr(G,E,T). %const
typeDec(G,fun(X,T,A,E),[(X,arrow(etoile(t1,etoile(t2,t3)), T) ) | G]) :- typeExpr([?|G],A)



typeCmds(_,[],void).
typeCmds(G,[S|CS],void) :- typeStat(G,S,void), typeCmds(G,CS,void). %STAT;CMDS
typeCmds(G,[D|CS],void) :- typeDec(G,D,NG), typeCmds(NG,CS,void).   %DEC;CMDS


typeStat(G,echo(X),void) :- typeExpr(G,X,int).

typeProg(_,prog(cmds(X)),void) :- typeCmds(_,X,void).