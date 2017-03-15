%% parser from Kent Universities Erlang Master Class.

-module(parser).

-export([print/1,eval/2,example_1/0, compile/1, run/2, parse/1, execute/2]).


-type expr() :: {'num',integer()}
              | {'var',atom()}
              | {'add',expr(),expr()}
              | {'mul',expr(),expr()}.

-spec example_1() -> expr().
example_1() ->
    {add,{num,2},{mul,{num,3},{num,4}}}.


%% pretty print an expression.
%% ex: {add,{var,a},{mul,{num,2},{var,b}}
%% should produce: "(a+ (2*b))"

-spec print(expr()) -> string().
print({num, N}) -> integer_to_list(N);
print({var, A}) -> atom_to_list(A);
print({add, LE, RE}) ->
    "(" ++ print(LE)++ "+" ++ print(RE) ++ ")";
print({mul, LE, RE}) ->
    "(" ++ print(LE)++ "-" ++ print(RE) ++ ")".


%% parsing
%% parse("(2+(3*4))") -> {add,{num,2},{mul,{num,3},{num.3}}}
%% parse("2") -> {num,2}
%% parse("a") -> {var,a}
%% parse("2+(3*4))") -> {num,2} + parse("+(3+4))")
%% return a pair {expression, what's left of input}.

-spec parse(string()) -> {expr(), string()}.

%% parse a fully bracked expression, with no spaces
%% '(' 'expression' 'op' 'expression' ')'
parse([$(|Rest]) ->                            % parse '('
      {E1,Rest1}     = parse(Rest),            % parse 'expression'
      [Op|Rest2]     = Rest1,                  % parse an 'op': '+' or '*'
      {E2,Rest3}     = parse(Rest2),           % parse another 'expression'
      [$)|RestFinal] = Rest3,                  % starts with a ')'
      {case Op of
	  $+ -> {add,E1,E2};
	  $* -> {mul,E1,E2}
        end,
       RestFinal};

%% parse an integer expression maybe starting with an '-'
parse([Ch|Rest]) when $0 =< Ch andalso Ch =< $9 orelse Ch==$- ->
    {Succeeds,Remainder} = get_while(fun is_digit/1, Rest),
    {{num, list_to_integer([Ch|Succeeds])}, Remainder};

%% parse an atom consisting of small letters only
parse([Ch|Rest]) when $a =< Ch andalso Ch =< $z ->
    {Succeeds,Remainder} = get_while(fun is_alpha/1, Rest),
    {{var, list_to_atom([Ch|Succeeds])}, Remainder}.


%% get the longest segment of a list with a given property P.
-spec get_while(fun((T) -> boolean()),[T]) -> {[T],[T]}.

get_while(P,[Ch|Rest]) ->
    case P(Ch) of
        true ->
            {Succeeds,Remainder} = get_while(P,Rest),
            [[Ch|Succeeds],Remainder];
        false ->
            {[],[Ch|Rest]}
    end;
get_while(_P,[]) ->
    {[],[]}.

%% test for alpha character
-spec is_alpha(integer()) -> boolean().
is_alpha(Ch) -> $a =< Ch andalso Ch =< $z.

%% test for digit character.
-spec is_digit(integer()) -> boolean().
is_digit(Ch) -> $0 =< Ch andalso Ch =< $9.


%% evaluating an expression
%% ex: {add,{num,2},{mul,{num,3},{num,4}}}
%% should produce: 14

-type env() :: [{atom(),integer()}].

-spec eval(env(),expr()) -> integer().

eval(_,{num, N}) -> N;
eval(Env,{var, A}) -> lookup(A,Env);
eval(Env,{add, LE, RE}) ->
     eval(Env,LE) + eval(Env,RE);
eval(Env,{mul, LE, RE}) ->
     eval(Env,LE) * eval(Env,RE).


%% lookup the value of a var in the envionment.
-spec lookup([{atom(),integer()}],atom()) -> integer().

lookup(A,[{A,N}|_]) -> N;
lookup(A,[_|Env]) -> lookup(A,Env).


%% Stack virtual machine.
%% runs a compiled list of istructiond one at a time

%% with instructions are one of:
%    {push, N} - push integer N onto the stack
%    {fetch, A} - lookup value of variable a and push the result onto the stack
%    {add2} - pop the top two elements of the stack, add, and push the result
%    {mul2} - pop the top two elements of the stack, multiply, and push the result

-type instr() :: {'push', integer()}
               | {'fetch', atom()}
               | {'add2'}
               | {'mul2'}.


-type program() :: [instr()].

-type stack() :: [integer()].

%% the compiler:
%% given: {add,{num,2},{mul,{num,3},{num,4}}}
%% produces: [{push,2},{push,3},{push,4},{mul2},{add2}]

-spec compile(expr()) -> program().

compile({num,N}) ->
    [{push,N}];
compile({var,A}) ->
    [{fetch,A}];
compile({add,E1,E2}) ->
    compile(E1) ++ compile(E2) ++ [{add2}];
compile({mul,E1,E2}) ->
    compile(E1) ++ compile(E2) ++ [{mul2}].


%% the interpreter. 
%% run a sequence (list) of mschine instructions, should only be
%% a correct sequence
%% given: [{push,2},{push,3},{push,4},{mul2},{add2}]
%% produces: 14

-spec run(program(), env()) -> integer().
run(Code,Env) ->
    run(Code,Env,[]).

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

%% compile and run an expression.
%% should produce the same results as eval().

-spec execute(env(),expr()) -> integer().
 
execute(Env, Expr) ->
    run(compile(Expr), Env).



%% TODO: simplification, subtraction, division, unary minus
%% zcc types booltan
