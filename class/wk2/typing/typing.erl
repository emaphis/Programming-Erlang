%% practice in statice typing in Erlang.

-module(typing).
-compile(export_all).

sum([]) ->
    0;
sum([H|T]) ->
    H + sum(T).

-spec foo(string() | integer()) -> atom() | integer().
foo(X) when is_integer(X) -> X + 1;
foo(X) -> list_to_atom(X).
