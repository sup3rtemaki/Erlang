-module(mdc).
-export([mdc/2]).


% Maximo Divisor Comum entre dois numeros inteiros
mdc(A, B) when is_integer(A), is_integer(B), B > A ->
  mdc(B, A);
mdc(A, 0) -> A;
mdc(A, B) -> mdc(A rem B, B).