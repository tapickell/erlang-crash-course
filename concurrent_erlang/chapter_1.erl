-module(chapter1).
-export([factorial/1, start/0, loop/0]).

factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).

double(X) -> times(X, 2).

times(X, N) -> X * N.

area({square, Side}) -> Side * Side;
area({rectangle, X, Y}) -> X * Y;
area({circle, Radius}) -> 3.14159 * Radius * Radius;
area({triangle, A, B, C}) -> 
  S = (A + B + C)/2,
  math:sqrt(S*(S-A)*(S-B)*(S-C)).

convert({farenheit, Temp}, celcius) ->
  {celcius, 5 * (Temp - 32) / 9};
convert({celcius, Temp}, farenheit) ->
  {farenheit, 32 + Temp * 9 / 5};
convert({reaumur, Temp}, celcius) ->
  {celcius, 10 * Temp / 8};
convert({celciusm Temp}, reaumur) ->
  {reaumur, 8 * Temp / 10};
convert({X, _}, Y) ->
  {cannot,convert,X,to,Y}.

start() ->
  spawn(echo, loop, []).

loop() ->
  receive
    {From, Message} ->
      From ! Message,
      loop()
  end.

Id = start(),
Id ! {self(), hello}.
