-module(rps2).

-export([play/1,echo/1,play_two/3,val/1,tournament/2]).

%% strategies
-export([rock/1, no_repeat/1, const/1, paper/1, rand/1, scissors/1, random_choice/1]).

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
-spec play_two(strategy(), strategy(), [play()], [play()], non_neg_integer()) -> integer().
play_two(_,_,PlaysL,PlaysR,0) ->
   tournament(PlaysL, PlaysR);

play_two(StrategyL,StrategyR,PlaysL,PlaysR,N) ->
   play_two(StrategyL,StrategyR,[StrategyL(PlaysL)|PlaysL], [StrategyR(PlaysL)|PlaysR], (N-1)).

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
rock2(Plays) ->
    (const(rock))(Plays).

-spec paper([play()]) -> play().
paper(Plays) ->
    (const(paper))(Plays).

-spec scissors([play()]) -> play().
scissors(Plays) ->
   (const(scissors))(Plays).

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

%% play the opponents most frequent play
-spec max_play([play()]) -> play().
max_play(Plays) ->
    {Rock,Paper,Scissors} = count_plays(Plays),
    if
        Rock >= Paper andalso Rock >= Scissors -> rock;
        Paper >= Rock andalso Paper >= Scissors -> paper;
        true -> scissors
    end.

%% play the opponents least frquent play.
-spec min_play([play()]) -> play().
min_play(Plays) ->
    {Rock,Paper,Scissors} = count_plays(Plays),
    if
        Rock =< Paper andalso Rock =< Scissors -> rock;
        Paper =< Rock andalso Paper =< Scissors -> paper;
        true -> scissors
    end.


max_min_test() ->
    ?assertEqual(rock, max_play([])),
    ?assertEqual(rock, min_play([])),
    ?assertEqual(paper, max_play([paper])),
    ?assertEqual(rock, min_play([paper])),
    ?assertEqual(rock, max_play([paper,rock,scissors])),
    ?assertEqual(rock, min_play([paper,rock, scissors])),
    ?assertEqual(scissors, max_play([scissors,rock,scissors,paper,scissors,paper])),
    ?assertEqual(rock, min_play([scissors,rock,scissors,paper,scissors,paper])).

    

%% count plays in a list, store totals in a tuple.
%% {rock, paper, scissors}.

-spec count_plays([play()]) ->
                         {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
count_plays(Plays) ->
    count_plays(Plays, {0,0,0}).

count_plays([],Acc) -> Acc;
count_plays([X|Xs],{Rock,Paper,Scissors}) ->
    case X of
        rock     -> count_plays(Xs, {Rock+1,Paper,Scissors});
        paper    -> count_plays(Xs, {Rock,Paper+1,Scissors});
        scissors -> count_plays(Xs, {Rock,Paper,Scissors+1})
    end.

count_plays_test() ->
    ?assertEqual({0,0,0}, count_plays([])),
    ?assertEqual({1,0,0}, count_plays([rock])),
    ?assertEqual({0,1,0}, count_plays([paper])),
    ?assertEqual({0,0,1}, count_plays([scissors])),
    ?assertEqual({1,2,3}, count_plays([scissors,rock,scissors,paper,scissors,paper])).
-spec play_list() -> [play()].
play_list() ->
    [echo, rock, paper, scissors, no_repeat, cycle, rand, max_play, min_play].

%% play random play:
%% choose an random choise from a list of stratagies.
-spec random_choice([play()]) -> play().
random_choice(_Plays) ->
    Choice = random:uniform(length(play_list())),
    (lists:nth(Choice,play_list())).

