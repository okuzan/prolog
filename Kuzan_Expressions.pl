z12_1(A, B, C):-
       d(A, B, C),!.


d(sin(G), X, A* cos(G)):-          d(G, X, A).
d(cos(G), X, A* -sin(G)):-         d(G, X, A).
d(tg(G), X, A* 1/ cos(G)^(-2)):-   d(G, X, A).
d(ctg(G), X, A* -1 / sin(G) ^ 2):- d(G, X, A).
d(e^(G), X, A* e^(G)):-            d(G, X, A).


% dx/dx->1
d(X,X,1):-!.

% dc/dx ->0
d(C,_,0):-atomic(C).

% d(-G)/dx ->-(dG/dx)
d(-G,X,-A):-d(G,X,A).

% d(G+V)/dx -> dG/dx+dV/dx
d(G+V,X,A+B):-d(G,X,A),d(V,X,B).

% d(G-V)/dx -> dG/dx-dV/dx
d(G-V,X,A-B):-d(G,X,A),d(V,X,B).

% d(cG)/dx -> c(dG/dx)
d(C*G,X,C*A):-atomic(C),C\=X,d(G,X,A),!.

% d(GV)/dx -> G(dV/dx)+V(dG/dx)
d(G*V,X,B*G+A*V):-d(G,X,A),d(V,X,B).

%d(G/V)/dx -> d(GV^-1)/dx
d(G/V,X,A):-d(G*V^(-1),X,A).

%d(G^c)/dx -> cG^(c-1)(dG/dx)
d(G^C,X,C*G^(C-1)*W):-atomic(C),C\=X,d(G,X,W).

%d(lnG)/dx -> G^(-1)(dG/dx)
d(log(G),X,A*G^(-1)):-d(G,X,A).

% d(G(V))/dx -> (dV/dG)*dG/dx
d(G_V_X,X,DV*DG):-
  G_V_X=..[_,_,V_X],
  d(G_V_X,V_X,DG),
  d(V_X,X,DV).

% Дані для тестування:
% ?- z12_1(sin(3*x), x, A).
% A = 3*1*cos (3*x).
% ?- z12_1(cos(3*x^2), x, A).
% A = 3*(2*x^(2-1)*1)* -sin(3*x^2).
% ?- z12_1(ctg(11*x),x, A).
% A = 11*1* - (sin (11*x))^ -2.
% ?- z12_1(tg(x),x, A).
% A = 1*(cos x)^ -2.