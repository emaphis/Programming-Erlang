%% market project - 2.25

-module(market).
-include_lib("eunit/include/eunit.hrl").
-export([return_db/0, return_sales/0, find_product/2, sales_list/2,
         count_sherries/1, print_bill/1, run/0]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% To run the demo:
%% > market:run().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% data definitions:
%%

%% 'product' is a tuple: {product_code, description, price}
%%  where: 'product_code' is a 4 digit 'integer()',
%%         'description'  is a 'string()',
%%         'price'  is an 'integer()' with an implied
%%                   2 position decimal.
%%

-type product_code() :: pos_integer().
-type description() :: string().
-type price() :: integer().
-type product() :: {product_code(), description(), price()}. 

%% The sample 'data base' is a 'list of product'
%%
-spec return_db() -> [product()]. 
return_db() ->
   [ {4719, "Fish Fingers" , 121},
     {5643, "Nappies" , 1010},
     {3814, "Orange Jelly", 56},
     {1111, "Hula Hoops", 21},
     {1112, "Hula Hoops (Giant)", 133},
     {1234, "Dry Sherry, 1lt", 540} ].


%% the sample 'sales' is a 'list of product_code' in an order
%%  where 'product_code' is a 4 digit 'integer()'
-spec return_sales() -> [product_code()].
return_sales() -> [1234,4719,3814,1112,11913,1234].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 'product' component access functions:
%%
-spec product_code(product()) -> product_code().
product_code({Product_code,_,_}) -> Product_code.

-spec description(product()) -> description().
description({_,Description,_}) -> Description.

-spec price(product()) -> price().
price({_,_,Price}) -> Price.

access_functions_test() ->
    Prd = {3814, "Orange Jelly", 56},
    ?assertEqual(3814, product_code(Prd)),
    ?assertEqual("Orange Jelly", description(Prd)),
    ?assertEqual(56, price(Prd)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% produces a product tuple given a product code, and
%% the product db. If product code not found return an
%% 'error' tuple.
-spec find_product(product_code(),[product()]) -> product().
find_product(Pc,[]) ->
    {Pc, "error", 0};
find_product(Pc, [P|Db]) ->
    case product_code(P) of
        Pc -> P;
        _  -> find_product(Pc, Db)
    end.

find_product_test() ->
    Db = return_db(),
    ?assertEqual({999, "error",  0}, find_product(999, Db)),
    ?assertEqual({3814, "Orange Jelly", 56}, find_product(3814, Db)),
    ?assertEqual({1234, "Dry Sherry, 1lt", 540}, find_product(1234, Db)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sales list - produce list of products
%% for a given sale.

-spec sales_list([product_code()], [product()]) -> [product()].
sales_list(Sales,ProductDb) -> sales_list(Sales,ProductDb,[]).
sales_list([], _, Acc) -> lists:reverse(Acc);
sales_list([S|Sales], Db, Acc) ->
     sales_list(Sales, Db, [find_product(S,Db) | Acc]).

sales_list__test() ->
    Sales = return_sales(),
    Db = return_db(),
    ?assertEqual([{1234,"Dry Sherry, 1lt",540},
                  {4719,"Fish Fingers",121},
                  {3814,"Orange Jelly",56},
                  {1112,"Hula Hoops (Giant)",133},
                  {11913,"error",0},
                  {1234,"Dry Sherry, 1lt",540}],
                 sales_list(Sales, Db)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% filter out bad products
%% produce a new sales list with bad products filter out
%%

-spec filter_bad([product()]) -> [product()].
filter_bad(Sales) ->
    lists:reverse(filter_bad(Sales, [])).
filter_bad([], Acc) -> Acc;
filter_bad([S|Sales], Acc) ->
   case is_product(S) of
       true  -> filter_bad(Sales, [S|Acc]);
       false -> filter_bad(Sales, Acc)
   end.

%% a predicate that returns true if "product" tuple is an good,
%% false if 'product' tuple is and error.
-spec is_product(product()) -> boolean().
is_product(Prd) ->
    case description(Prd) of
        "error" -> false;
        _        -> true
    end.

filter_product_test() ->
    Db  = return_db(),
    Sales = return_sales(),
    Sales_list = sales_list(Sales, Db),
    ?assertEqual(
       [{1234,"Dry Sherry, 1lt",540},
        {4719,"Fish Fingers",121},
        {3814,"Orange Jelly",56},
        {1112,"Hula Hoops (Giant)",133},
        {1234,"Dry Sherry, 1lt",540}],
       filter_bad(Sales_list)).

is_product_test() ->
    ?assertEqual(true, is_product({3814, "Orange Jelly", 56})),
    ?assertEqual(false, is_product({9999, "error", 0})).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sherry discount handling
%%
%% for every 2 sherries deduct 1 pound sterling.
%% returns the number of sherries in a salses list
%% 1234 is the sherry 'product_code'.

-spec count_sherries([product()]) -> non_neg_integer().
count_sherries(Sales) ->
    count_sherries(Sales, 0).
count_sherries([], Acc)      -> Acc;
count_sherries([S|Sales], Acc) ->
    case product_code(S) of
        1234 -> count_sherries(Sales, 1+Acc);  % magic number!
        _    -> count_sherries(Sales, Acc)
    end.

%% calulate the discout given the list of sales
%% for every 2 sherries accumulate 1 pound.
-spec calc_discount([product()]) -> price().
calc_discount(Sales) ->
    100 * (count_sherries(Sales) div 2).

discounting_test() ->
    Db  = return_db(),
    Sales = return_sales(),
    Sales_list = sales_list(Sales, Db),
    ?assertEqual(0, count_sherries([])),
    ?assertEqual(2, count_sherries(Sales_list)),
    ?assertEqual(0, calc_discount([])),
    ?assertEqual(100, calc_discount(Sales_list)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sum the sales for an order. subtracting the discounts,
%% given the list of sales.
%%
-spec sum_sales([product()]) -> price().
sum_sales(Sales) ->
    sum_sales(Sales, 0) - calc_discount(Sales).

sum_sales([], Acc) -> Acc;
sum_sales([S|Sales], Acc) ->
    sum_sales(Sales, price(S) + Acc).

sum_sales_test() ->
    Db  = return_db(),
    Sales = return_sales(),
    Sales_list = sales_list(Sales, Db),
    ?assertEqual(0, sum_sales([])),   % base case
    ?assertEqual(1290,sum_sales(Sales_list)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Bill Printing
%% probably should be in its own module.
%%

%% print the bill to out put given a sales list.
%% filter out bad items first.
print_bill(Sales) ->
    print_header(),
    print_details(Sales),
    print_discount(Sales),
    print_total(Sales).

print_header() ->
    io:format("~n         Erlang Stores~n~n").

print_details([]) -> io:format("~n");

print_details([S|Sales]) ->
    io:format("  ~-20s  ~8.2f ~n", [description(S), price(S)/100]),
    print_details(Sales).

print_discount(Sales) ->
    Disc = calc_discount(Sales),
    io:format("  Discount:             ~8.2f~n~n", [0-Disc/100]).

print_total(Sales) ->
    Total = sum_sales(Sales),
    io:format("  Total Bill            ~8.2f~n~n", [Total/100]).


%% Demo the print_bill function
run() ->
    Db  = return_db(),
    Sls = return_sales(),
    Sales = filter_bad(sales_list(Sls, Db)),
    print_bill(Sales).


%% Produces this output:

%%
%%         Erlang Stores
%%
%%  Dry Sherry, 1lt           5.40
%%  Fish Fingers              1.21
%%  Orange Jelly              0.56
%%  Hula Hoops (Giant)        1.33
%%  Dry Sherry, 1lt           5.40
%%
%%  Discount:                -1.00
%%
%%  Total Bill               12.90
%%
%%ok
%%
