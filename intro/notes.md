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
