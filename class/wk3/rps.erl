%% Modeling a rock paper scissors tournament  --   3.8

-module(rps).

-export([beat/1, lose/1, result/2, score/1, tournament/2]).

-include_lib("eunit/include/eunit.hrl").

-type rps() :: rock | paper | scissors | error.
-type result() :: win | lose | draw.


%% what beats my move?
-spec beat(rps()) -> rps().
beat(rock) -> paper;
beat(paper) -> scissors;
beat(scissors) -> rock;
beat(_) -> error.

%% what loses to my move?
-spec lose(rps()) -> rps().
lose(rock) -> scissors;
lose(paper) -> rock;
lose(scissors) -> paper;
lose(_) -> error.

%% who wins from a right hand perspective
-spec result(rps(), rps()) -> result().
result(rock, scissors) -> win;
result(paper, rock) -> win;
result(scissors, paper) -> win;
result(_, error) -> win;  % right wins by default
result(rock, paper) -> lose;
result(paper, scissors) -> lose;
result(scissors, rock) -> lose;
result(error,_) -> lose; % left wins by defualt
result(_, _) -> draw.


-spec score(result()) -> integer().
score(win)  ->  1;
score(draw) ->  0;
score(lose) -> -1.


-spec tournament([rps()],[rps()]) -> integer().
tournament(Xs, Ys) ->
    Results = lists:zipwith(fun result/2, Xs, Ys),
    lists:sum(lists:map(fun score/1, Results)).

tournament_test() ->
    ?assertEqual(0, tournament([rock,rock,rock], [rock,rock,rock])),
    ?assertEqual(-1, tournament([rock,rock,paper,paper],
                                [rock,paper,scissors,rock])).
