parent(tom,bob).
parent(tom,liz).
parent(bob,ann).
parent(bob,pat).
parent(pat,jim).
parent(pam,bob).
f(pat).
f(ann).
f(liz).
m(tom).
m(jim).
m(bob).
offspring(X,Y) :- parent(Y,X).
grandparent(X,Z) :- parent(X,Y), parent(Y,Z).
sis( X, Y) :-
        parent( Z, X),
        parent( Z, Y),
        f( X).

forebear( X, Z) :-
		parent( X, Z).

forebear( X, Z) :-
		parent( X, Y),
		forebear( Y, Z).