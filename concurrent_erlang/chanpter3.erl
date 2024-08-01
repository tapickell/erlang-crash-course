-module(chapter3).

member(X, [X|_]) -> true;
member(X, [_|T]) -> member(X, T);
member(_X, []) -> false.

append([H|L1], L2) -> [H|append(L1 ,L2)];
append([], L)) -> L.

reverse(L) ->
  reverse(L, []).

reverse([H|T], L) ->
  reverse(T, [H|L]);
reverse([], L) -> 
  L.

delete_all(X, [X|T]) ->
  delete_all(X, T);
delete_all(X, [Y|T]) ->
  [Y|delete_all(X, T)];
delete_all(_, []) -> [].

quick_sort([]) -> [];
quick_sort([Pivot|Tail]) ->
  {Smaller, Bigger} = split(Pivot, Tail),
  append(quick_sort(Smaller), quick_sort(Bigger)).

split(Pivot, L) ->
  split(Pivot, L, [], []).

split(Pivot, [], Smaller, Bigger) ->
  {Smaller, Bigger};
split(Pivot, [H|T], Smaller, Bigger) when H < Pivot ->
  split(Pivot, T, [H|Smaller], Bigger);
split(Pivot, [H|T], Smaller, Bigger) when H >= Pivot ->
  split(Pivot, T, Smaller, [H|Bigger]).

