%% consolidate list knowledge - 2.18
%% to run all tests: consolidate:test_all().

-module(consolidate).
-export([join/2, concat/1, member/2, merge_sort/1, quick_sort/1,
         insertion_sort/1]).
-export([join_test/0, concat_test/0, member_test/0, merge_sort_test/0,
         merge_test/0, split_test/0, quick_sort_test/0, partition_test/0,
         insertion_sort_test/0, insert_test/0, test_all/0]).

%% join returns a new string joining 2 given strings
join(Str1, Str2) -> join2(lists:reverse(Str1), Str2).
join2([], Str) -> Str;
join2([X|Xs], Str) -> join2(Xs, [X|Str]).

join_test() ->
    [] = join("", ""),
    "hello" = join("hello", ""),
    "hello" = join("", "hello"),
    "hello" = join("hel", "lo"),
    ok.


%% concat returns a string joining strings in a list of strings.
concat(Lst) -> concat(lists:reverse(Lst), []).
concat([], Acc) -> Acc;
concat([X|Xs], Acc) ->
    concat(Xs, join(X,Acc)).

concat_test() ->
    "hello world" = concat(["hello ", "world"]),
    "goodbye" = concat(["goo","d","","by","e"]),
    ok.

%% member returns 'true' if an element is contained in a list,
%% false otherwise
member(_, [])     -> false;
member(X, [X|_])  -> true;
member(Y, [_|Xs]) -> member(Y, Xs).

member_test() ->
    false = member(2, []),
    true  = member(2, [2,0,0,1]),
    false = member(20, [2,0,0,1]),
    true  = member(0, [2,0,0,1]),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% merge_sort - http://en.wikipedia.org/wiki/Merge_sort
%%
merge_sort([])  -> [];
merge_sort([X]) -> [X];
merge_sort(Xs) ->
    {Lft, Rgt} = split(Xs),
    merge(merge_sort(Lft), merge_sort(Rgt)).

split(Xs) -> split(Xs, [], []).
split([],  Lft, Rgt) ->  {Lft, Rgt};
split([X], Lft, Rgt) ->  {[X|Lft], Rgt};
split([X,Y|Zs], Lft, Rgt) ->
    split(Zs, [X|Lft], [Y|Rgt]).

merge(Xs,[]) -> Xs;
merge([],Ys) -> Ys;
merge([X|Xs],[Y|Ys]) ->
    case X =< Y of
        true  -> [X|merge(Xs, [Y|Ys])];
        false -> [Y|merge([X|Xs], Ys)]
    end.

merge_sort_test() ->
    [1,2,3,4,5,6,7,8,9] = merge_sort([5,3,9,4,1,6,8,2,7]),
    ok.

split_test() ->
    {[],[]}       = split([]),
    {"a",[]}      = split("a"),
    {"ca","b"}    = split("abc"),
    {"ca","db"}   = split("abcd"),
    {"eca", "db"} = split("abcde"),
    ok.

merge_test() ->
    []       = merge([],[]),
    "abc"    = merge("abc",[]),
    "def"    = merge([],"def"),
    "abcdef" = merge("abc", "def"),
    "abcdef" = merge("bce","adf"),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% quick sort -- http://en.wikipedia.org/wiki/Quicksort
%%
quick_sort([]) -> [];
quick_sort([Pv|Xs]) ->
    {Lt,Gt} = partition(Pv,Xs),
    concat([quick_sort(Lt), [Pv], quick_sort(Gt)]).

partition(Pv, Xs) -> partition(Pv, Xs, [], []).
partition(_Pv, [], Lt, Gt) -> {Lt, Gt};
partition(Pv, [X|Xs], Lt, Gt) ->
    case X =< Pv of
        true  -> partition(Pv, Xs, [X|Lt], Gt);
        false -> partition(Pv, Xs, Lt, [X|Gt])
    end.

quick_sort_test() ->
    [1,2,3,4,5,6,7,8,9] = quick_sort([5,3,9,4,1,6,8,2,7]),
    ok.

partition_test() ->
    {"cba","gfed"} = partition($c, "abcdefg"),
    {"dcba","gfe"} = partition($d, "abcdefg"),
    {"edcba","gf"} = partition($e, "abcdefg"),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% insertion sort -- http://en.wikipedia.org/wiki/Insertion_sort
%%
insertion_sort([]) -> [];
insertion_sort([X|Xs]) ->
    insert(X, insertion_sort(Xs)).

insert(X, []) -> [X];
insert(X, [Y|Ys]) ->
    case X =< Y of
        true   -> [X | [Y|Ys]]; % X < Ys no need to recurse.
        false  -> [Y | insert(X, Ys)]
    end.

insertion_sort_test() ->
    [1,2,3,4,5,6,7,8,9] = insertion_sort([5,3,9,4,1,6,8,2,7]),
    ok.

insert_test() ->
    "a"    = insert($a, []),  % base case
    "ab"   = insert($a, "b"),
    "abc"  = insert($b, "ac"),
    "abcd" = insert($c, "abd"),
    ok.

test_all() ->
    ok = join_test(),
    ok = concat_test(),
    ok = member_test(),
    ok = merge_sort_test(),
    ok = merge_test(),
    ok = split_test(),
    ok = quick_sort_test(),
    ok = partition_test(),
    ok = insertion_sort_test(),
    ok = insert_test(),
    ok.
