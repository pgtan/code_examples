% Problem Euler: 9
:- use_module(library(clpfd)).

% brute force
euler9(A,B,C,Vs) :-
    Vs = [A,B,C],
    Vs ins 1..998,
    A^2 + B^2 #= C^2,
    A #< B,
    A + B + C #= 1000.

% Dickson's Method by Josef Rukavicka
rukavicka(K,P,Q,Vs) :-
    Vs = [K,P,Q],
    Vs ins 1..498,
    K #> Q,
    P + Q #> K,
    Q^2 #= 2*P*(K - Q),
    2*K + 2*P + Q #= 1000.
