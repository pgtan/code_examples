%%% Project Euler Problem 15
:- use_module(library(clpfd)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% stolen from https://www.metalevel.at/prolog/memoization %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic memo_/1.
memo(Goal) :-
        (   memo_(Goal)
        ->  true
        ;   once(Goal),
            assertz(memo_(Goal))
        ).

lattice_paths(1,1,N) :- N #= 0.

lattice_paths(1,_,N) :- N #= 1.

lattice_paths(_,1,N) :- N #= 1.

lattice_paths(X,Y,N) :-
    Y in 1..21,
    X in 1..21,
    X1 is X - 1,
    Y1 is Y - 1,
    memo(lattice_paths(X1,Y,N1)),
    memo(lattice_paths(X,Y1,N2)),
    N is N1 + N2.

