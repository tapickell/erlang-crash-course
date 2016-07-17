-module(recursive).
-export([fac/1, len/1, tail_fac/1, tail_len/1, dup/2, tail_dup/2, rvrs/1, tail_rvrs/1, sublist/2]).

fac(0) -> 1;
fac(N) when N > 0 -> N*fac(N-1).

len([]) -> 0;
len([_|T]) -> 1 + len(T).

tail_len(L) -> tail_len(L,0).

tail_len([], Acc) -> Acc;
tail_len([_|T], Acc) -> tail_len(T, Acc+1).

tail_fac(N) -> tail_fac(N,1).

tail_fac(0,Acc) -> Acc;
tail_fac(N,Acc) when N > 0 -> tail_fac(N-1, N*Acc).

dup(0,_) -> [];
dup(N,Term) when N > 0 -> [Term|dup(N-1, Term)].

tail_dup(N,Term) -> tail_dup(N, Term, []).

tail_dup(0,_,List) -> List;
tail_dup(N,Term,List) when N > 0 -> tail_dup(N-1, Term, [Term|List]).

rvrs([]) -> [];
rvrs([H|T]) -> rvrs(T)++[H].

tail_rvrs(L) -> tail_rvrs(L,[]).

tail_rvrs([],Acc) -> Acc;
tail_rvrs([H|T],Acc) -> tail_rvrs(T, [H|Acc]).

sublist(_,0) -> [];
sublist([],_) -> [];
sublist([H|T],N) when N > 0 -> [H|sublist(T,N-1)].

