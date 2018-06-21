:- use_module(library(clpfd)).

euler4(A,B,C,D1,D2,Vs) :-
    Vs = [A,B,C],
    Vs ins 0..9,
    D1 in 100..999,
    D2 in 100..999,
    A #\= 0,
    D1 * D2 #= A + 10*B + 100*C +1000*C + 10000*B + 100000*A.
