%% test code for unit testing practice

-module(mylist).
-export([sum/1,product/1,odds/1]).


sum([]) -> 0;
sum([X|XS]) -> X + sum(XS).

product([]) -> 1;
product([X|XS]) -> X * product(XS). 


odds(List) -> odds(List,1).
odds([X|R],N) when N rem 2 == 1 -> [X | odds(R,N+1)];
odds([_|R],N) -> odds(R,N+1);
odds([],_) -> [].
