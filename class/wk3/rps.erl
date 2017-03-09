%% Modeling a rock paper scissors tournament  --   3.8,3.9

-module(rps).

-export([beat/1, lose/1, result/2, score/1, tournament/2, enum/1, val/1]).

%% strategies
-export([echo/1, rock/1, no_repeat/1, const/1, cycle/1, rand/1]).

%% game play interface.
-export([play/1]).

-include_lib("eunit/include/eunit.hrl").


%% types:
-type play() :: rock | paper | scissors | error.
-type result() :: win | lose | draw.


%% what beats my move?
-spec beat(play()) -> play().
beat(rock) -> paper;
beat(paper) -> scissors;
beat(scissors) -> rock;
beat(_) -> error.

%% what loses to my move?
-spec lose(play()) -> play().
lose(rock) -> scissors;
lose(paper) -> rock;
lose(scissors) -> paper;
lose(_) -> error.

%% who wins from a right hand perspective
-spec result(play(), play()) -> result().
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


-spec tournament([play()],[play()]) -> integer().
tournament(Xs, Ys) ->
    Results = lists:zipwith(fun result/2, Xs, Ys),
    lists:sum(lists:map(fun score/1, Results)).

tournament_test() ->
    ?assertEqual(0, tournament([rock,rock,rock], [rock,rock,rock])),
    ?assertEqual(-1, tournament([rock,rock,paper,paper],
                                [rock,paper,scissors,rock])).



%% transform 0, 1, 2 to rock, paper, scissors and vice versa.

enum(0) -> rock;
enum(1) -> paper;
enum(2) -> scissors.

val(rock)     -> 0;
val(paper)    -> 1;
val(scissors) -> 2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% strategies.

-type strategy() :: fun(([play()]) -> play()).

-spec echo([play()]) -> play().
echo([]) -> paper;
echo([S|_]) -> S.

-spec rock([play()]) -> play().
rock(_) -> rock.

-spec no_repeat([play()]) -> play().
no_repeat([]) -> scissors;
no_repeat([X|_]) -> beat(X).

%-spec const([play()]) -> play().
%-spec const(play()) -> fun ([play()]) -> move()).
-spec const([play()]) -> fun(([play()]) -> play()).
const(Play) ->
    fun(_) -> Play() end.

-spec cycle([play()]) -> play().
cycle(Xs) ->
    enum(length(Xs) rem 3).

-spec rand([play()]) -> play().
rand(_) ->
    enum(random:uniform(3) - 1).



%
% interactively play against a strategy, provided as argument.
%

-spec play(strategy()) -> ok.
play(Strategy) ->
    io:format("Rock - paper - scissors~n"),
    io:format("Play one of rock, paper, scissors, ...~n"),
    io:format("... r, p, s, stop, followed by '.'~n"),
    play(Strategy,[]).

% tail recursive loop for play/1
-spec play(strategy(), [play()]) -> ok.
play(Strategy,Moves) ->
    {ok,P} = io:read("Play: "),
    Play = expand(P),
    case Play of
        stop ->
            io:format("Stopped~n");
        _    ->
            Result = result(Play,Strategy(Moves)),
            io:format("Result: ~p~n",[Result]),
            play(Strategy,[Play|Moves])
    end.

%% transform shorthand atoms to expended forms
expand(r) -> rock;
expand(p) -> paper;
expand(s) -> scissors;
expand(X) -> X.
