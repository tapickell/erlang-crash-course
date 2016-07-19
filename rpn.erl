-module(rpn).
-export([  calculate/1
         , test/0
        ]).


calculate(L) when is_list(L) ->
  [Res] = lists:foldl(fun rpn/2, [], string:tokens(L, " ")),
  Res.

rpn("+", [N1,N2 | Stack]) -> [N2+N1 | Stack];
rpn("-", [N1,N2 | Stack]) -> [N2-N1 | Stack];
rpn("*", [N1,N2 | Stack]) -> [N2*N1 | Stack];
rpn("/", [N1,N2 | Stack]) -> [N2/N1 | Stack];
rpn("^", [N1,N2 | Stack]) -> [math:pow(N2,N1) | Stack];
rpn("ln", [N | Stack])    -> [math:log(N) | Stack];
rpn("log10", [N | Stack]) -> [math:log10(N) | Stack];
rpn("sum", Stack)  -> [lists:foldl(fun(X,Y) -> X + Y end, 0, Stack)|[]];
rpn("prod", Stack) -> [lists:foldl(fun(X,Y) -> X * Y end, 1, Stack)|[]];
rpn(X, Stack) -> [read(X) | Stack].

read(N) ->
  case string:to_float(N) of
    {error,no_float} -> list_to_integer(N);
    {F,_} -> F
  end.

test() ->
  5 = calculate("2 3 +"),
  87 = calculate("90 3 -"),
  -4 = calculate("10 4 3 + 2 * -"),
  -2.0 = calculate("10 4 3 + 2 * - 2 /"),
  ok = try
    calculate("90 34 12 33 55 66 + * - +")
  catch
    error:{badmatch,[_|_]} -> ok
  end,
  4037 = calculate("90 34 12 33 55 66 + * - + -"),
  8.0 = calculate("2 3 ^"),
  true = math:sqrt(2) == calculate("2 0.5 ^"),
  true = math:log(2.7) == calculate("2.7 ln"),
  true = math:log10(2.7) == calculate("2.7 log10"),
  50 = calculate("10 10 10 20 sum"),
  10.0 = calculate("10 10 10 20 sum 5 /"),
  1000.0 = calculate("10 10 20 0.5 prod"),
  ok.
