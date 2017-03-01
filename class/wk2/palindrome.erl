%% palendrome definition - 2.15, 2.16

-module(palindrome).
-author('emaphis').

-export([palindrome/1, only_letters/1]).
-export([palindrome_test/0, reverse_test/0, only_letters_test/0]).

palindrome(Xs) ->
    Ys = string:to_lower(only_letters(Xs)),
    Ys == reverse(Ys).

palindrome_test() ->
    true  = palindrome(""),
    true  = palindrome("a"),
    true  = palindrome("aa"),
    false = palindrome("abb"),
    true  = palindrome("aba"),
    true  = palindrome("Aba"),
    true  = palindrome("AbCdcba"),
    true  = palindrome("Madam I\'m Adam"),
    ok.

reverse(Xs) -> reverse(Xs, []).
reverse([], Acc) -> Acc;
reverse([X|Xs], Acc) ->
    reverse(Xs, [X|Acc]).

reverse_test() ->
    []    = reverse([]),
    [1]   = reverse([1]),
    "abc" = reverse("cba"),
    ok.


only_letters(Str) ->
    lists:reverse(only_letters(Str, [])).
only_letters([], Acc) -> Acc;
only_letters([X|Xs], Acc) ->
    case ($a =< X andalso $z >= X) orelse
         ($A =< X andalso $Z >= X) of
        true  -> only_letters(Xs, [X|Acc]);
        false -> only_letters(Xs, Acc)
    end.

only_letters_test() ->
    "a"  = only_letters("a"),
    "abc"  = only_letters("abc"),
    "abc"  = only_letters("a b c"),
    "abc"  = only_letters("a%b%c"),
    "ABC"  = only_letters("A,B,C"),
    ok.
