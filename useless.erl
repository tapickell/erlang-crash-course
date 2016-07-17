-module(useless).
-author("@tapickell <tapickell@gmail.com>").
-export([add/2, hello/0, greet_and_add_two/1, greet/2]).

add(A,B) ->
  A + B.

%% Shows greeting.
%% io:format/1 is the standard function used to output test.
hello() ->
  io:format("Hello, world!~n").

greet_and_add_two(X) ->
  hello(),
  add(X,2).

greet(male, Name) ->
  io:format("Hello, Mr. ~s!", [Name]);
greet(female, Name) ->
  io:format("Hello, Mrs. ~s!", [Name]);
greet(_, Name) ->
  io:format("Hello, ~s!", [Name]).




%% Think in concurrent processes

%% If circular dependencies are disgusting in real life,
%% maybe they should be just as disgusting in your programs too.
