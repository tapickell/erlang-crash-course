-module(guards).
-author("@tapickell <tapickell@gmail.com>").
-export([old_enough/1, right_age/1, wrong_age/1]).

old_enough(X) when is_integer(X), X >= 16 -> true;
old_enough(_) -> false.

right_age(X) when is_integer(X), X >= 16, X =< 85 -> true;
right_age(_) -> false.

wrong_age(X) when is_integer(X), X < 16; X > 85 -> true;
wrong_age(_) -> false.



%% , == andalso
%% ; == orelse
