%% chapter 5 - user interface
-module(ask_5).
-export([line/0]).

%% Collecting user response a line at a time.

line() ->
    Planemo = get_planemo(),
    Distance = get_distance(),
    drop:fall_velocity({Planemo, Distance}).


get_planemo() ->
    io:format("Where are you?~n"),
    io:format(" 1. Earth ~n"),
    io:format(" 2. Earth's Moon ~n"),
    io:format(" 3. Mars ~n"),
    Answer =  io:get_line("Which? > "),

    Value = hd(Answer),  % grab first char in string
    char_to_planemo(Value).

char_to_planemo(Char) ->
    if
        [Char] == "1" -> earth;
        Char == $2 -> moon;
        Char == 51 -> mars
    end.

get_distance() ->
    Input = io:get_line("How far? (meters)"),
    Value = string:strip(Input, right, $\n),
    {Distance, _} = string:to_integer(Value),
    Distance.
