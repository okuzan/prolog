%
is_end(A, B) :- append(_, A, B).


secondLast(L, X) :-
    append(_, [X, _], L).

%% передОстанній(L, X) :-
%%     злиття(_, [X, _], L).

reverse2([],[]).
reverse2([H|T],R):- reverse2(T,R2), append(R2,[H],R).

%% інверсія([],[]).
%% інверсія([H|T],R):- інверсія(T,R2), злиття(R2,[H],R).
male(ivan).
male(petro).
male(sasha).
male(nick).

female(masha).
female(kate).
female(nata).
female(sasha).
female(dasha).



accRev([H|T],A,R):-  accRev(T,[H|A],R).
accRev([],A,A).
rev(L,R):-  accRev(L,[],R).
