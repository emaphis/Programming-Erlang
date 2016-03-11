%% tut2 -- exercising atoms

-module(tut2).

-export([convert/2]).


%% length conversion
convert(M, inch) ->
    M / 2.54;
convert(N, centimeter) ->
    N * 2.54.
