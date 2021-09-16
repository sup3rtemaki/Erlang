 %% quicksort:qsort(List)
 %% Classificar uma lista de itens
 -module(quicksort).
 -export([qsort/1]).

 qsort([]) -> [];
 qsort([Pivot|Rest]) ->
     qsort([X || X <- Rest, X < Pivot]) ++ [Pivot] ++ qsort([Y || Y <- Rest, Y >= Pivot]).
