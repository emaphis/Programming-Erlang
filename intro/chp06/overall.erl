%% Chapter 6 - Lists
-module(overall).
-export([product/1]).

%% Calculating the product of values in a list.
%% tail call recursive.

product([])   -> 0;   % return 0 for empty list
product(List) -> product(List, 1).

product([], Product) -> Product;  % the base case
product([Head|Tail], Product) -> product(Tail, Product * Head).
