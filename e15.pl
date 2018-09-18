% Project Euler 15
:- use_module(library(tabling)).

:- table lattice_paths/3.


lattice_paths(1,1,N) :- N is 0.

lattice_paths(1,Y,N) :-
    Y > 1,
    N is 1.

lattice_paths(X,1,N) :-
    X > 1,
    N is 1.

lattice_paths(X,Y,N) :-
    X > 0,
    Y > 0,
    X1 is X - 1,
    Y1 is Y - 1,
    lattice_paths(X1,Y,N1),
    lattice_paths(X,Y1,N2),
    N is N1 + N2.
