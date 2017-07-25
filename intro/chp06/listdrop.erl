%% Chapter 6 - Lists
-module(listdrop).
-export([falls/1]).


falls(List) -> lists:reverse(falls(List, [])).

falls([], Results) -> Results;
falls([Head|Tail], Results) ->
    falls(Tail, [drop:fall_velocity(Head) | Results]).
