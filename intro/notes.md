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

* create Erlang documentation
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
lists:flatten(Full).
lists:append(A, B).
++   ;; append.
lists:reverse(List).
lists:zip(Lst1,Lst2).
{Lst1,Lst2} = lists:unzip(List).
lists:foreach(Function,List).
lists:map(Function,List).
lists:filter(Predicate,List).
lists:all(Predicate,List).
lists:partition(Compare,List).

;; list comprehensions
[Function(Value) || Value <- List].
[Function(Value) || Value <- List, Predicate].  ;; filter


* processes
** primatives
- spawn(Mod, Fun, Args).
Proc = spawn(foo, bar, []).

- ! - send a message - add a message to a mailbox
Pid ! Msg

- self(). return process id.

- receive  - receive a message - remove a message from the mailbox

receive
  Msg1 -> handle_message1();
  Msg2 -> handle_message2()
after Time -> handle_timeout()
end.

pid/3

;; how to pass the pid
; send message with pid

Server ! {self(), ping}.

receive
  {Pid, ping} -> Pid ! pong
end.

; Servers pid as part of the reply

Server ! {self(), ping}.
receive
  {Server, pong}  -> hdl_pong().
end.

receive
  {Pid, ping} -> Pid ! {self(), pong}
end.


; Register a process - named process
Server = spawn(M,F,[]).
register(server, Server).
server ! {self(), ping}.

register(server, spawn(M,F,[])).  % one step.

Pid = whereis(Name).

; sending a message to a non-existent name is an error...
; ... the assumption is that a named process should not disappear.
; typically we name "static", long-lived processes.

; if you send a message before server is registered you have
; a race condition
