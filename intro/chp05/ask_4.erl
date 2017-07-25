%% chapter 5 - user interface
-module(ask_4).
-export([chars/0]).

%% Presenting a menu and waiting for a single character response.
chars() ->
    io:format("Which planemo are you on?~n"),
    io:format(" 1. Earth ~n"),
    io:format(" 2. Moon ~n"),
    io:format(" 3. Mars ~n"),
    io:get_chars("Which? > ", 1).
