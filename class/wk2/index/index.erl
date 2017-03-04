-module(index).
-export([get_file_contents/1,show_file_contents/1]).
-export([index_file/1,partition/1]).
-export([partition_test/0]).

%% The main prorgram.
%% Run:
%%   index:index_file("gettysburg-address.txt").
%% to produce a list of word indexes.
index_file(File) ->
    Lines = get_file_contents(File),
    Words = process_lines(Lines),
    collect_words(Words).

%% Count and process each line of text, produces a list of {Word, Line_Number} pairs.
process_lines(Lines) -> process_lines(Lines, 0, []).
process_lines([], _Count, Acc) -> lists:sort(Acc);
process_lines([L|Ls], Count, Acc) -> 
    Next = Count+ 1,
    process_lines(Ls, Next, index_line(L, Next, Acc)). 

%% Given a line of text, split line into words and produce a list of Word, Line_number pairs.
index_line(Line, Count, Acc) ->
    Toks = string:tokens(Line, " \t\n\b\r\\.,:;!?=-"),
    Words = clean_words(Toks),
    index_words(Words, Count, Acc).

%% Given a list of words produces a list of lowercase words above a certain length.
clean_words([])     -> [];
clean_words([X|Xs]) ->
    case string:len(X) > 3  of
        true  -> [string:to_lower(X) | clean_words(Xs)];
        false -> clean_words(Xs)
    end.


%% Given a list of Words and a Line_number, produce a list of Word, Line_Number pairs.
index_words([], _, Acc) -> Acc;
index_words([T|Ts], Count, Acc) ->
    index_words(Ts, Count, [{T, Count} | Acc]).

%% Given a sorted list of {Word, Line_Numbers} pairs collect into a list of {Word, List-0f-Line-Numbers}.
collect_words(Wrds) -> lists:sort(collect_words(Wrds, [], [])).
collect_words([{Wrd,Ln}], Acc, Idx) ->
    [{Wrd,lists:sort([Ln|Acc])}|Idx];
collect_words([{Wrd,Ln1},{Wrd,_Ln2}|Wrds], Acc, Idx) ->
    collect_words(Wrds,[Ln1|Acc], Idx);
collect_words([{Wrd1,Ln1},{_Wrd2,_Ln2}|Wrds], Acc, Idx) ->
    Index = partition([Ln1|Acc]),
    collect_words(Wrds, [], [{Wrd1, Index}|Idx]). 


%% partition a list of numbers into a list of Number Range Pairs.
%% See test function for examples.
partition(Lst) -> partition(lists:sort(Lst), []).

partition([], Acc) -> lists:reverse(Acc);
partition([X|Xs], [{X}|Acc]) ->
	partition(Xs, [{X}|Acc]);
partition([X|Xs], [{Start}|Acc]) when X == Start+1 ->
	partition(Xs, [{Start,X}|Acc]);
partition([X|Xs], [{Start,X}|Acc]) ->
	partition(Xs, [{Start,X}|Acc]);
partition([X|Xs], [{Start,End}|Acc]) when X == End+1 ->
	partition(Xs, [{Start,X}|Acc]);
partition([X|Xs], Acc) ->
	partition(Xs, [{X}|Acc]).

partition_test() ->    
    [{1}] = partition([1]),
    [{1}] = partition([1,1,1]),
    [{1,2}] = partition([2,1,1]),
    [{1,3}] = partition([3,2,2,1,1]),
    [{1,3},{5}] = partition([5,5,3,2,2,1]),
    [{1},{3},{5,7}] = partition([7,7,6,5,3,1,1]),
    ok.



% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)


% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_contents(Name) ->
    {ok,File} = file:open(Name,[read]),
    Rev = get_all_lines(File,[]),
    lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.
get_all_lines(File,Partial) ->
    case io:get_line(File,"") of
        eof -> file:close(File),
               Partial;
        Line -> {Strip,_} = lists:split(length(Line)-1,Line),
                get_all_lines(File,[Strip|Partial])
    end.

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

show_file_contents([L|Ls]) ->
    io:format("~s~n",[L]),
    show_file_contents(Ls);
show_file_contents([]) ->
    ok.    
     
