-module(chapter2).
-export([reverse/1])

classify_day(saturday) -> weekEnd;
classify_day(sunday) -> weekEnd;
classify_day(_) -> weekDay;

reverse(L) ->
  reverse(L, []).

reverse([H|T], L) ->
  reverse(T, [H|L]);
reverse([], L) -> 
  L.

what_is_it(input) ->
  case input of
    i when is_atom(i) -> 'Atom';
    i when is_integer(i) -> 'Integer';
    i when is_list(i) -> 'List';
    i when is_pid(i) -> 'Pid';
    i when is_tuple(i) -> 'Tuple';
    i when is_binary(i) -> 'Binary';
    _ -> 'Not Sure'
  end.
