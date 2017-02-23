%% a database program

-module(db).

-export([new/0, write/3, delete/2, read/2, match/2]).
-include_lib("eunit/include/eunit.hrl").


new() -> [].

new_test_() ->
    [?_assertEqual([], new())].


write(Key, Element, Db) ->
    [{Key, Element} | Db].

write_test_() ->
    Db = new(),
    Db2 = write(francesco, london, Db),
    Db3 = write(lelle, stockholm, Db2),
    Db4 = write(joern, paris, Db3),
    [?_assertEqual([], Db),
     ?_assertEqual([{francesco,london}], Db2),
     ?_assertEqual([{lelle,stockholm},{francesco,london}], Db3),
     ?_assertEqual([{joern,paris},{lelle,stockholm},{francesco,london}], Db4)].


delete(_, []) -> [];
delete(Key, [{Key,_} | T]) -> delete(Key, T);
delete(Key, [H|T]) -> [H | delete(Key, T)].

delete_test_() ->
    Db = new(),
    Db2 = write(francesco, london, Db),
    Db3 = write(lelle, stockholm, Db2),
    Db4 = write(joern, paris, Db3),
    Db5 = delete(lelle, Db4),
    Db6 = delete(joern, Db5),
    [?_assertEqual([{joern,paris},{lelle,stockholm},{francesco,london}], Db4),
     ?_assertEqual([{joern,paris},{francesco,london}], Db5),
     ?_assertEqual([{francesco,london}], Db6)].


read(_, []) -> {error, instance};
read(Key, [{Key, Element} | _]) -> {ok, Element};
read(Key, [_ | T]) -> read(Key, T).

read_test_() ->
    Db = new(),
    Db2 = write(francesco, london, Db),
    Db3 = write(lelle, stockholm, Db2),
    Db4 = write(joern, stockholm, Db3),
    [?_assertEqual([{joern,stockholm},{lelle,stockholm},{francesco,london}], Db4),
     ?_assertEqual({error,instance}, read(ola, Db4)),
     ?_assertEqual({ok,stockholm}, read(lelle,Db4)),
     ?_assertEqual({ok,london}, read(francesco,Db4))].


match(_, []) -> [];
match(Element, [{Key, Element} | T]) -> [Key | match(Element, T)];
match(Element, [_ | T]) -> match(Element, T).

match_test_() ->
    Db = new(),
    Db2 = write(francesco, london, Db),
    Db3 = write(lelle, stockholm, Db2),
    Db4 = write(joern, stockholm, Db3),
    [?_assertEqual([{joern,stockholm},{lelle,stockholm},{francesco,london}], Db4),
     ?_assertEqual([francesco], match(london, Db4)),
     ?_assertEqual([joern, lelle], match(stockholm, Db4))].
