%% chapter 5 - user input
-module(ask_2).
-export([term/0]).


%% Asking the user for an Erlang term
term() ->
    Input = io:read("What {planemo, distance} ? >>"),
    process_term(Input).


process_term({ok, Term}) when is_tuple(Term) ->
    drop:fall_velocity(Term);
process_term({ok, _}) ->
    io:format("You must enter a tuple");
process_term({error, _}) ->
    io:format("You must enter a tuple with correct syntax").
