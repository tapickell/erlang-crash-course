-module(recursive).
-export([  fac/1
         , len/1
         , tail_fac/1
         , tail_len/1
         , dup/2
         , tail_dup/2
         , rvrs/1
         , tail_rvrs/1
         , sublist/2
         , tail_sublist/2
         , zip/2
         , tail_zip/2
         , l_zip/2
         , tail_l_zip/2
         , quicksort/1
         , lc_quicksort/1
        ]).

%% Factorial of Integer
fac(0) -> 1;
fac(N) when N > 0 -> N*fac(N-1).

tail_fac(N) -> tail_fac(N,1).

tail_fac(0,Acc) -> Acc;
tail_fac(N,Acc) when N > 0 -> tail_fac(N-1, N*Acc).

%% Length of List
len([]) -> 0;
len([_|T]) -> 1 + len(T).

tail_len(L) -> tail_len(L,0).

tail_len([], Acc) -> Acc;
tail_len([_|T], Acc) -> tail_len(T, Acc+1).

%% List of Duplicated Integers
dup(0,_) -> [];
dup(N,Term) when N > 0 -> [Term|dup(N-1, Term)].

tail_dup(N,Term) -> tail_dup(N, Term, []).

tail_dup(0,_,List) -> List;
tail_dup(N,Term,List) when N > 0 -> tail_dup(N-1, Term, [Term|List]).

%% List Reversal
rvrs([]) -> [];
rvrs([H|T]) -> rvrs(T)++[H].

tail_rvrs(L) -> tail_rvrs(L,[]).

tail_rvrs([],Acc) -> Acc;
tail_rvrs([H|T],Acc) -> tail_rvrs(T, [H|Acc]).

%% Sublist of List
sublist(_,0) -> [];
sublist([],_) -> [];
sublist([H|T],N) when N > 0 -> [H|sublist(T,N-1)].

tail_sublist(L,N) -> lists:reverse(tail_sublist(L,N,[])).

tail_sublist(_, 0, SL) -> SL;
tail_sublist([], _, SL) -> SL;
tail_sublist([H|T], N, SL) when N > 0 -> tail_sublist(T, N-1, [H|SL]).

%% Zip Two Matched Lists into List of Tuples
zip([],[]) -> [];
zip([X|Xs], [Y|Ys]) -> [{X,Y} | zip(Xs,Ys)].

tail_zip(X,Y) -> lists:reverse(tail_zip(X,Y,[])).

tail_zip([],[],List) -> List;
tail_zip([X|Xs], [Y|Ys], List) -> tail_zip(Xs, Ys, [{X,Y}|List]).

%% Zip Two Lists into List of Tuples
%% Throw away remainder of unmatched list
l_zip([],_) -> [];
l_zip(_,[]) -> [];
l_zip([X|Xs], [Y|Ys]) -> [{X,Y} | l_zip(Xs,Ys)].

tail_l_zip(X,Y) -> lists:reverse(tail_l_zip(X,Y,[])).

tail_l_zip([],_,List) -> List;
tail_l_zip(_,[],List) -> List;
tail_l_zip([X|Xs], [Y|Ys], List) -> tail_l_zip(Xs, Ys, [{X,Y}|List]).

%% Quick Sort naive as the pivot is just head
quicksort([]) -> [];
quicksort([Pivot|Rest]) ->
  {Smaller, Larger} = partition(Pivot, Rest, [], []),
  quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

partition(_,[], Smaller, Larger) -> {Smaller, Larger};
partition(Pivot, [H|T], Smaller, Larger) ->
  if H =< Pivot -> partition(Pivot, T, [H|Smaller], Larger);
     H >  Pivot -> partition(Pivot, T, Smaller, [H|Larger])
  end.

lc_quicksort([]) -> [];
lc_quicksort([Pivot|Rest]) ->
  lc_quicksort([Smaller || Smaller <- Rest, Smaller =< Pivot])
  ++ [Pivot] ++
  lc_quicksort([Larger || Larger <- Rest, Larger > Pivot]).



















