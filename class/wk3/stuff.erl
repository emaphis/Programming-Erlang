-module(stuff).

-export([nth/2,why/0]).

%nth(0,[X|_]) -> X;
%nth(N,[_|Xs]) -> nth(N-1,Xs);
%nth(_N,_Xs) -> 0.

nth(0,[X|_]) -> X;
nth(N,[_|Xs]) -> nth(N-1,Xs).

%nth(N,[_|Xs]) -> nth(N-1,Xs);
%nth(0,[X|_]) -> X.


why() ->
    case [2,3,4] of 
        [X,Y|_] -> X+Y; 
        [S] -> S; 
        _ -> 0 
    end.
