* commands
pwd().  -- present working directory
cd("..").  -- change directory
cd("usr/emaphis").
f().  % forget
b().  % bound variables.

* functions
rem div
round().
math:sin().
math:cos().
math:pow(2,16).


edoc:files(["drop.erl"], [{dir, "doc"}]).

;; atoms - self evaluate
-- used in types and pattern matching.
atom
atom_2
'This is an atom'


;; tuples
Tuple = {earth, 20}.
element(2, Tuple).
Tuple2 = setelement(2, Tuple, 40).
tuple_size(Tuple2).

;; string  - linked list of characters
io:format("~w string~n" [String])
"erl" ++ "ang".
string:concat("erl", "ang").
"erl" == "ang"
"erl" =:= "erl"  exact equality
string:equal/2
re: regular expressions
Value = string:strip(Input, right, $\n),
{Distance, _} = string:to_integer(Value), ;; a monad!!

;; io
io:read().
io:get_chars("Which? > ", 1).
io:get_line("Which? > ", 1),


;; lists
lists:nth(1, Answer).  ;; first item is '1' not '0'.
