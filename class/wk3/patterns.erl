%% patterns in Erlang code.

-module(patterns).

-export([sum/1,double/1, map/2, double_m/1,
         greater_than_2/1, filter/2, greater_than_2_f/1,
         reduce/3, sum_r/1, sum_l/1]).

-include_lib("eunit/include/eunit.hrl").

%% List is either empty [] or non-empty.
%% the non-empty case matches [X|Xs]
%% where X matches the first element.
%%       Xs matches the rest of the list

%% foo([])     -> ... ;
%% foo([X|Xs]) -> ... X ... F(Xs) ... .

%% combining all of the elements of a list (reduce).
%% transforming all of the elements of the list. (map)
%% selecting elements of a list. (filter).


%% map

twice(N) -> 2*N.

double([]) -> [];
double([X|Xs]) -> [twice(X) | double(Xs)].

-spec map(fun((A) -> B),[A]) -> [B].
map(_,[]) -> [];
map(F, [X|Xs]) -> [ F(X) | map(F, Xs)].

double_m(Xs) -> map(fun twice/1, Xs).


gt_2(X) -> X > 2.

greater_than_2([]) -> [];
greater_than_2([X|Xs]) ->
    case gt_2(X) of
        true  -> [X | greater_than_2(Xs)];
        false -> greater_than_2(Xs)
    end.

-spec filter(fun((A)-> boolean()),[A]) -> [A].
filter(_,[]) -> [];
filter(P,[X|Xs]) ->
    case P(X) of
        true  -> [X | filter(P,Xs)];
        false -> filter(P,Xs)
    end.

greater_than_2_f(Xs) -> filter(fun gt_2/1, Xs).


%% reduce
add(X,Y) -> X+Y.

sum([]) -> 0;
sum([X|Xs]) -> add(X, sum(Xs)).

reduce(_,S,[]) -> S;
reduce(C,S,[X|Xs]) -> C(X, reduce(C,S,Xs)).

sum_r(Xs) -> reduce(fun add/2, 0, Xs).


%% using lamdas
sum_l(Xs) -> reduce(fun (X,Y)->X+Y end, 0, Xs).
