-module(index).
-export([get_file_contents/1,show_file_contents/1]).
-export([index_file/1]).

index_file(File) ->
    Lines = get_file_contents(File),
    Words = process_lines(Lines),
    lists:reverse(Words).

process_lines(Lines) -> process_lines(Lines, 0, []).
process_lines([], _Count, Acc) -> Acc;
process_lines([L|Ls], Count, Acc) ->
    Next = Count+ 1,
    process_lines(Ls, Next, process_line(L, Next, Acc)). 

process_line(Line, Count, Acc) ->
    Toks = string:tokens(Line, " ,.-"),
    process_tokens(Toks, Count, Acc).

process_tokens([], _, Acc) -> Acc;
process_tokens([T|Ts], Count, Acc) ->
    process_tokens(Ts, Count, [{Count,T} | Acc]).



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
     
