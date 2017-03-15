%% parser from Kent Universities Erlang Master Class.

-module(parser).

-export([print/1,eval/2,example_1/0, compile/1, run/3]).


-type expr() :: {'num',integer()}
              | {'var',atom()}
              | {'add',expr(),expr()}
              | {'mul',expr(),expr()}.

-type env() :: [{atom(),integer()}].

-type instr() :: {'push', integer()}
               | {'fetch', atom()}
               | {'add2'}
               | {'mul2'}.

-type program() :: [instr()].

-type stack() :: [integer()].

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

%% Stack virtual machine.
%% Push N instruction pushes the integer N onto the top of the stac,

-spec compile(expr()) -> program().
compile({num,N}) ->
    [{push,N}];
compile({var,A}) ->
    [{fetch,A}];
compile({add,E1,E2}) ->
    compile(E1) ++ compile(E2) ++ [{add2}];
compile({mul,E1,E2}) ->
    compile(E1) ++ compile(E2) ++ [{mul2}].



-spec run(program(), env(), stack()) -> integer().
run([{push, N} | Continue], Env, Stack) ->
    run(Continue, Env, [N|Stack]);
run([{fetch, A} | Continue], Env, Stack) -> 
    run(Continue, Env, [lookup(A,Env)|Stack]);
run([{add2} | Continue], Env, [N1,N2|Stack]) ->
    run(Continue, Env, [(N1+N2)|Stack]);
run([{mul2} | Continue], Env, [N1,N2|Stack]) ->
    run(Continue, Env, [(N1*N2)|Stack]);
run([],_Env,[N]) ->
    N.

