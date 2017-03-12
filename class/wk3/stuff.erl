-module(stuff).

-export([nth/2]).

%nth(0,[X|_]) -> X;
%nth(N,[_|Xs]) -> nth(N-1,Xs);
%nth(_N,_Xs) -> 0.

nth(0,[X|_]) -> X;
nth(N,[_|Xs]) -> nth(N-1,Xs).

%nth(N,[_|Xs]) -> nth(N-1,Xs);
%nth(0,[X|_]) -> X.
