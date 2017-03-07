%% typer experiments.

-module(palin).
-compile(export_all).

-spec palindrome(string()) -> boolean().
palindrome(Xs) ->
    palin(nocaps(nopunct(Xs))).

-spec nopunct(string()) -> string().
nopunct([]) -> [];
nopunct([X|Xs]) ->
    case lists:member(X,".,-\'\"\'") of
        true  ->
            nopunct(Xs);
        false ->
            [X|nopunct(Xs)]
    end.

-spec nocaps([atom()]) -> [atom()].
nocaps([]) -> [];
nocaps([X|Xs]) ->
    [ nocap(X) | nocaps(Xs) ].

-spec nocap(_) -> any().
nocap(X) ->
    case $A =< X andalso X =< $Z of
        true  ->
            X+32;
        false ->
            X
    end.

-spec palin(string()) -> boolean().
palin(Xs) ->
    Xs == reverse(Xs).

-spec reverse([any()]) -> any().
reverse(Xs) ->
    shunt(Xs, []).

-spec shunt([any()],_) -> any().
shunt([], Acc) -> Acc;
shunt([X|Xs], Acc) ->
    shunt(Xs, [X|Acc]).

-spec test() -> none().
test() ->
    palindrome(["madam"]).
