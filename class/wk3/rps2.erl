-module(rps2).

-export([play/1,echo/1,play_two/3,val/1,tournament/2]).

%% strategies
-export([rock/1, no_repeat/1, const/1, rock2/1, paper/1, scissors/1, random_choice/1]).

-include_lib("eunit/include/eunit.hrl").

%% data specs
-type play() :: rock | paper | scissors.
-type result() :: win | lose | draw.
-type strategy() :: fun(([play()]) -> play()).

%
% play one strategy against another, for N moves.
%

play_two(StrategyL,StrategyR,N) ->
    play_two(StrategyL,StrategyR,[],[],N).

% tail recursive loop for play_two/3
% 0 case computes the result of the tournament

% FOR YOU TO DEFINE
% REPLACE THE dummy DEFINITIONS

play_two(_,_,PlaysL,PlaysR,0) ->
   dummy;

play_two(StrategyL,StrategyR,PlaysL,PlaysR,N) ->
   dummy.

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

%
% auxiliary functions
%

% transform shorthand atoms to expanded form

-spec expand(any()) -> any().
expand(r) -> rock;
expand(p) -> paper;		    
expand(s) -> scissors;
expand(X) -> X.

% result of one set of plays

-spec result(play(), play()) -> result().
result(rock,rock) -> draw;
result(rock,paper) -> lose;
result(rock,scissors) -> win;
result(paper,rock) -> win;
result(paper,paper) -> draw;
result(paper,scissors) -> lose;
result(scissors,rock) -> lose;
result(scissors,paper) -> win;
result(scissors,scissors) -> draw.

% result of a tournament

-spec tournament([play()],[play()]) -> integer().
tournament(PlaysL,PlaysR) ->
    lists:sum(
      lists:map(fun outcome/1,
		lists:zipwith(fun result/2,PlaysL,PlaysR))).

-spec outcome(result()) -> integer().
outcome(win)  ->  1;
outcome(lose) -> -1;
outcome(draw) ->  0.

% transform 0, 1, 2 to rock, paper, scissors and vice versa.

-spec enum(pos_integer()) -> play().
enum(0) ->
    rock;
enum(1) ->
    paper;
enum(2) ->
    scissors.

-spec val(play()) -> pos_integer().
val(rock) ->
    0;
val(paper) ->
    1;
val(scissors) ->
    2.

% give the play which the argument beats.

-spec beats(play()) -> play().
beats(rock) ->
    scissors;
beats(paper) ->
    rock;
beats(scissors) ->
    paper.

%
% strategies.
                                                %
-spec echo([play()]) -> play().
echo([]) ->
     paper;
echo([Last|_]) ->
    Last.

echo_test() ->
    ?assertEqual(paper,     echo([paper,scissors,rock])),
    ?assertEqual(scissors,  echo([scissors, paper, rock])),
    ?assertEqual(rock,      echo([rock, scissors, paper])).


-spec rock([play()]) -> play().
rock(_) ->
    rock.

rock_test() ->
    ?assertEqual(rock, rock([paper, rock, paper])).

% FOR YOU TO DEFINE
% REPLACE THE dummy DEFINITIONS

-spec no_repeat([play()]) -> play().
no_repeat([]) ->
    rock;
no_repeat([X|_]) ->
    beats(X).

no_repeat_test() ->
    ?assertEqual(rock,      no_repeat([paper,scissors,rock])),
    ?assertEqual(paper,     no_repeat([scissors, paper, rock])),
    ?assertEqual(scissors,  no_repeat([rock, scissors, paper])).

%% create constant strategies.
-spec const(play()) -> fun (([play()]) -> play()).
const(Play) ->
    fun (_) -> Play end.

-spec rock2([play()]) -> play().
rock2(Xs) ->
    (const(rock))(Xs).

-spec paper([play()]) -> play().
paper(Xs) ->
    (const(paper))(Xs).

-spec scissors([play()]) -> play().
scissors(Xs) ->
   (const(scissors))(Xs).

const_test() ->
  ?assertEqual(rock, rock2([rock,paper,scissors])),
  ?assertEqual(paper, paper([rock,paper,scissors])),
  ?assertEqual(scissors, scissors([rock,paper,scissors])).


-spec cycle([play()]) -> play().
cycle(Xs) ->
    enum(length(Xs) rem 3).

cycle_test() ->
    ?assertEqual(rock,     cycle([])),   % base case
    ?assertEqual(paper,    cycle([rock])),
    ?assertEqual(scissors, cycle([rock,rock])),
    ?assertEqual(rock,     cycle([rock,rock,rock])).


-spec rand([play()]) -> play().
rand(_) ->
    enum(random:uniform(3)-1).


%% random choice:
%% choose an random choise of stratagy
-spec random_choice([play()]) -> play().
random_choice(Xs) ->
    case random:uniform(8) of
        1 -> echo(Xs);
        2 -> rock(Xs);
        3 -> no_repeat(Xs);
        4 -> rock2(Xs);
        5 -> paper(Xs);
        6 -> scissors(Xs);
        7 -> cycle(Xs);
        8 -> rand(Xs)
    end.
