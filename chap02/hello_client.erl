-module(hello_client).
-export([greet/1, leave/1]).

greet(Server) ->
    Server ! {self(), hello},
    receive
        {Server, Message} ->
            Message
    end.

leave(Server) ->
    Server ! {self(), goodbye},
    receive
        {Server, Message} ->
            Message
    end.
