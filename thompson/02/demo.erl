-module(demo).
-export([double/1,area/1]).
-import(math, [sqrt/1]).



% This is a comment.
% Everything on a line after % is ignored.

double(Value) ->
    times(Value, 2).

times(X,Y) ->
    X*Y.

area({triangle, A, B, C}) ->
    S = (A + B +C)/2,
    sqrt(S*(S-A)*(S-B)*(S-C)).


