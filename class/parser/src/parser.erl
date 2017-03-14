%% parser from Kent Universities Erlang Master Class.

-module(parser).

-export([print/1,eval/2,example_1/0]).

-type expr() :: {'num',integer()}
              | {'var',atom()}
              | {'add',expr(),expr()}
              | {'mul',expr(),expr()}.

-type env() :: [{atom(),integer()}].

-spec example_1() -> expr().
example_1() ->
    {add,{num,2},{mul,{num,3},{num,4}}}.


%% pretty print an expression.
-spec print(expr()) -> string().
print({num, N}) -> integer_to_list(N);
print({var, A}) -> atom_to_list(A);
print({add, LE, RE}) ->
    "(" ++ print(LE)++ "+" ++ print(RE) ++ ")";
print({mul, LE, RE}) ->
    "(" ++ print(LE)++ "-" ++ print(RE) ++ ")".


-spec eval(env(),expr()) -> integer().
eval(_,{num, N}) -> N;
eval(Env,{var, A}) -> lookup(A,Env);
eval(Env,{add, LE, RE}) ->
     eval(Env,LE) + eval(Env,RE);
eval(Env,{mul, LE, RE}) ->
     eval(Env,LE) * eval(Env,RE).

-spec lookup([{atom(),integer()}],atom()) -> integer().
lookup(A,[{A,N}|_]) -> N;
lookup(A,[_|Env]) -> lookup(A,Env).


%% Stack virtual achine.
%% Push N instruction pushes the integer N onto the top of the stac,

