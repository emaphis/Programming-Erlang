%% chapter 5 - user input
-module(ask_1).
-export([term/0]).


%% Asking the user for an Erlang term
term() ->
    Input = io:read("What {planemo, distance} ? >>"),
    Term = element(2,Input),
    drop:fall_velocity(Term).
