-module(b_sort).
 
-export([bubble_sort/1]).

bubble_sort(L) -> bubble_sort(L,len(L)).

bubble_sort(L,1) -> io:format("Resultado: ~p~n",[L]);
bubble_sort([H|T],N) -> 
    R = bubble(H,T),
    io:format("Sorteando: ~p~n",[R]),
    bubble_sort(R,N-1).

bubble(H,[]) -> [H];
bubble(X,[H|T]) ->
    if X > H ->
            [H|bubble(X,T)];
        true ->
            [X|bubble(H,T)]
    end.
 
len([]) -> 0;
len([_H|T]) -> 1 + len(T).