%побудова 
parse(K ^ 2, R) :- parse(K, K1),  operate(K1, R). % піднесення дужки до 2 степеня
parse(K ^ N, R) :- integer(N), parse(K, K1),  operate(K1, N, ^, R). %піднесення одночлена до степеня

parse(K + N, R) :- integer(N), integer(K), R is K + N. %додавання цілих чисел
parse(K - N, R) :- integer(N), integer(K), R is K - N. %віднімання цілих чисел
parse(K * N, R) :- integer(N), integer(K), R is K * N. %множення цілих чисел

parse(N*x^P, M) :- integer(P), integer(N), M = mono(coef(N), pow(P)). %повна форма одночлена
%скорочені форми для зручності
parse(x^P, M) :- integer(P), M = mono(coef(1), pow(P)). %коефіцієнт 1
parse(N*x, M) :- integer(N), M = mono(coef(N), pow(1)). %степінь 1
parse(x, M) :- M = mono(coef(1), pow(1)). %коефіцієнт та степінь 1
parse(N, M) :- integer(N), M = mono(coef(N), pow(0)). %степінь 0, просто ціле число

parse(K + G, R) :- parse(K, K1), parse(G, G1), operate(K1, G1, +, R). %додавання многочленів
parse(K - G, R) :-  parse(K, K1), parse(G, G1), operate(K1, G1, -, R). %віднімання многочленів
parse(K * G, R) :- parse(K, K1), parse(G, G1), operate(K1, G1, *, R). %множення многочленів

%mono is short for monomial, одночлен. op(l([])) - список доданків, членів многочлена

%піднесення двочлена до степеня, тільки для двох доданків (а^2 + 2ab + b^2)
operate(op(l([H1|[H2|_]])), R) :-   operate(H1, 2, ^, A), %квадрат першого
                                    operate(H2, 2, ^, B), %квадрат другого
                                         operate(A, B, *, AB),   %АБ
                                         operate(mono(coef(2),pow(0)), AB, *, M),  %2АБ
                                         R = op(l([A, B, M])). %результат - 3 доданки
%піднесення одночлена до степеня
operate(mono(coef(N1), pow(K1)), P, ^, R) :-  N is N1 ^ P, 
                                              K is K1 * P,
                                              R = mono(coef(N), pow(K)).

%множення одночленів
operate(mono(coef(N1), pow(K1)),
        mono(coef(N2), pow(K2)), *, R) :-  N is N1 * N2, 
                                           K is K1 + K2, 
                                           R = mono(coef(N), pow(K)).

%список додати до списку - об'єднати
operate(op(l(List1)), op(l(List2)), +, R) :-  append(List1, List2, L), R = op(l(L)).

%список мінус список - об'єднати, змінити знаки у доданках другого списку
operate(op(l(List1)), op(l(List2)), -, R) :-  maplist(negate, List2, List2N), 
                                              append(List1, List2N, L), R = op(l(L)).

%список мінус одночлен, змінити знак і об'єднати
operate(op(l(List)), mono(coef(N1), pow(K1)), -, R) :- B is N1*(-1), % не можна просто дописати мінус
                                                       negate(mono(coef(B), pow(K1)), M),
                                                       R = op(l([M|List])).

%перемноження списків
operate(op(l(List1)), op(l(List2)), *, R) :- maplist(mull2(op(l(List1))), List2, L), 
                                             flatten(L, F), R = op(l(F)).

%додавання одночленів однакового степеня - одночлен
operate(mono(coef(N1), pow(K)), mono(coef(N2), pow(K))
	, +, R) :-  N is N1+N2, R = mono(coef(N), pow(K)).

%віднімання одночленів однакового степеня - одночлен
operate(mono(coef(N1), pow(K)), mono(coef(N2), pow(K))
	, -, R) :-  N is N1-N2, R = mono(coef(N), pow(K)).

%додавання одночленів різних степенів - многочлен, не спрощується
operate(mono(coef(N1), pow(K1)), mono(coef(N2), pow(K2))
	, +, R) :-  K1 \= K2, 
     R = op(l([mono(coef(N1), pow(K1)), mono(coef(N2), pow(K2))])).

%віднімання одночленів різних степенів - многочлен, не спрощується
operate(mono(coef(N1), pow(K1)), mono(coef(N2), pow(K2))
	, -, R) :-  K1 \= K2, 
     R = op(l([mono(coef(N1), pow(K1)), mono(coef(-N2), pow(K2))])).

%додавання одночлена до многочлена - многочлен
operate(op(l(List)), mono(coef(N1), pow(K1)), +, R) :-  M = mono(coef(N1), pow(K1)), R = op(l([M|List])).

%множення одночлена на многочлен, дужку
operate(mono(coef(N1), pow(K1)), op(l(List)), *, R) :- 
maplist(mull(mono(coef(N1), pow(K1))), List, L), R = op(l(L)).

%множення многочлена, дужки на одночлен - просто інший порядок вхідних
operate(op(l(List)), mono(coef(N1), pow(K1)), *, R) :- 
operate(mono(coef(N1), pow(K1)), op(l(List)), *, R).

%домножити одночлена на мінус одиницю
negate(mono(coef(N), pow(P)), R) :- B is N*(-1), R = mono(coef(B), pow(P)).

%допоміжна функція, перефразування для зручного використання operate в maplist
mull(mono(coef(N1), pow(K1)), mono(coef(N2), pow(K2)), R) :- 
operate(mono(coef(N1), pow(K1)), mono(coef(N2), pow(K2)), *, R).

%допоміжна функція, перефразування для зручного використання operate в maplist
mull2(op(l(List)), mono(coef(N1), pow(K1)), R) :- 
maplist(mull(mono(coef(N1), pow(K1))), List, L), R = L.

%перетворити список списків у просто список.
flatten(L, R) :- reverse(L, L1), flatten2(L1, R). %оригінальна змінює порядок
flatten2([], _).
flatten2([H|T], R) :- foldl(append, T, H, R).

%вивід, якщо мінус - не добавляти плюс
show_records([]).
show_records([A | B]) :-
  \+ checky(A),
  format(' + ~w', A),
  show_records(B).

show_records([A | B]) :-
  checky(A),
  format(' ~w', A),
  show_records(B).

checky(K*x^_) :- K < 0.
checky(K*x) :- K < 0.

%форматований вивід для різних одночленів
outs(mono(coef(K), pow(P)), [K]) :-  P == 0.
outs(mono(coef(K), pow(P)), [x]) :- K == 1, P == 1.
outs(mono(coef(K), pow(_)), [0]) :- K == 0.
outs(mono(coef(K), pow(P)), [x^P]) :- K == 1.
outs(mono(coef(K), pow(P)), [K*x]) :- P == 1.
outs(mono(coef(K), pow(P)), [K*x^P]). %загальний варіант


%представлення списку, сортування, перерахунок (на випадок доданків однакового степеня)
outs(op(l(List)), R) :- maplist(outs, List, R1), flatten(R1, R2), my_sort(R2, R3), 
                          maplist(parse, R3, R4), maplist(outs, R4, R5), flatten(R5, R6), my_sort(R6, R).	

%у сортованому списку попарні перевірки, чи можливе спрощення
concise(L, R) :- foldl(aux, L, mono(coef(0), pow(0)), R).

%допоміжна, на випадок доданків однакового степеня
aux(G, K, R) :- parse(G, G1), operate(K, G1, +, R).

%сортування списку одночленів, працює для повної форми
my_sort(List, R) :- sort(2, @>=, List, R).  % sort list of polynomials

%основна функція, створення многочленів, обрахунки, вивід
z12_2(E, R) :- parse(E, R1), outs(R1, R), show_records(R).


%3 завдання
z12_3(Expr, X, R1, A) :- z12_2(Expr, Poly), maplist(der, Poly, R1), %перетворюємо у список одночленів (2 завдання), обчислюємо похідну одночленів
                         maplist(calc(X), R1, R), foldl(plus, R, 0, A). %підставляємо значення та обраховуємо
calc(X, K*x^P, R) :- R is K * (X ^ P).
der(K1*x^P1, K2*x^P2) :- K2 is K1*P1, P2 is P1 - 1.

der_each(List, R) :- maplist(der, List, R).


%Проміжне тестування
% op(l([mono(coef(5), pow(4), coef(2), pow(2))])).
% 2*3*x^4 + (5*x^4)*(2*x^3) + 2*x*(10*x^5) - 5*x
%  (x^3 + x)*(2*x^4 + 4*x^2) + 2*3*x^4 + (5*x^4)*(2*x^3) + 2*x*(10*x^5) - 5*x
% op(l([mono(coef(- 5), pow(1)), mono(coef(20), pow(6)), mono(coef(6), pow(4)), mono(coef(10), pow(7))])
%  [10*x^7, 20*x^6, 6*x^4, - 5*x^2]

%Тестові приклади

% ? - z12_2(4*x + 5*x, R).
% ? - z12_2(4*x + 5*x^4, R).
% ? - z12_2(4 + 5 + 44, R).
% ? - z12_2((3*x^2)^2, R).
% ? - z12_2((2*x^2 + 3*x^2)^2, R).
% ? - z12_2((2*x^3 + 3*x^2)^2, R).
% ? - z12_2((((3*x^5)^3 + 2*x^4)^2 - ( 42 + 7*x)* (5*x^3 + 6*x)), R).
% ? - z12_3((2*x^2 + 3*x^2)^2, R, 2, A).
% ? - z12_3((x^3 + x)*(2*x^4 + 4*x^2) + 2*3*x^4 + (5*x^4)*(2*x^3) + 2*x*(10*x^5), R, 1, A).
% ? - z12_3((x^3 + x)*(2*x^4 + 4*x^2) + 2*3*x^4 - (5*x^4)*(2*x^3) + 2*x*(10*x^5), R, 3, A).


%----
%%Інша спроба імплементації
%% z12_2(K + N, R) :- integer(N), integer(K), R is K + N.
%% z12_2(K - N, R) :- integer(N), integer(K), R is K - N.
%% z12_2(K * N, R) :- integer(N), integer(K), R is K * N.

%% z12_2(N*x^P + 0, N*x^P).
%% z12_2(0 + N*x^P, N*x^P).

%% z12_2(N*x^P - F*x^P, R) :- integer(F), integer(N), K is (N - F), R = K*x^P.
%% z12_2(N*x^P + F*x^P, R) :- integer(F), integer(N), K is (N + F), R = K*x^P.

%% z12_2(N*x^P * F*x^P, R) :- integer(F), integer(N), K is (N * F), H is 2*P, R = K*x^H.
%% z12_2(K*x * (N*x^P), F) :- integer(N), integer(P), P1 is P + 1, M is K * N, F = M*x^P1.

%% %% z12_2(N*x*(G), F) :- z12_2(G, D), write("a"), F = z12_2(N*x*D).
%% %% z12_2(N*x^P + F*x^P2, R) :- R = [N*x^P, F*x^P2]. %() to list


%% z12_2((N*x^P) + K, R) :- integer(N), integer(K),  integer(P), R = N*x^P + K.
%% z12_2((N*x^P) - K, R) :- integer(N), integer(K),  integer(P), R = N*x^P - K.
%% z12_2((N*x^P) * K, R) :- integer(N), integer(K),  integer(P), H is N * K, R = H*x^P.
%% z12_2(K * (N*x^P), R) :- integer(N), integer(K),  integer(P), H is N * K, R = H*x^P.

%% z12_2((N*x^P) + (F*x^P), R) :- integer(F), integer(N), integer(P), K is (N + F), R = K*x^P.
%% z12_2((N*x^P) - (F*x^P), R) :- integer(F), integer(N),  integer(P), K is (N - F), R = K*x^P.
%% z12_2((N*x^P1) * (F*x^P2), R) :- integer(F), integer(N), integer(P1), integer(P2),
%%                                    K is (N * F), P is (P1 + P2), R = K*x^P.

%% %% z12_2(N*x^P, R) :- integer(P), integer(N), R = N*x^P. %повернути одночлен

%% z12_2(N*x^P, [N*x^P]) :- integer(P), integer(N). %повернути одночлен


%% mult(A, B, C) :- z12_2((A) * (B), (C)).
%% z12_2((N*x^P), List, R) :- integer(P), integer(N), write("e"), maplist(mult((N*x^P)), List, R).

%% z12_2(K * G, R) :- K1 = z12_2((K), (K1)), G1 = z12_2((G), (G1)), R = z12_2((K1) * (G1)).
%% %% z12_2((K) * G, R) :- z12_2((K) * (G), R).
%% %% z12_2(K+G, R) :- R = z12_2(K1+G1), K1 = z12_2(K, K1), G1 = z12_2(G, G1).
  
%% %% z12_2(K + (G), R) :- z12_2(K, K1), z12_2(G, G1), 
%% %%                          my_sort([(G)|K1], R).
%% %% z12_2(K - (G), R) :- z12_2(K, K1), z12_2(G, G1), 
%% %%                          my_sort([(-G)|K1], R).


%% %(5*x^2 + 5*x^3 + 5*x^4).
%% %% z12_2(K - (G), R) :- z12_2(K, K1), z12_2(G, G1), 
%% %%                          append([-G], K1, R).

%% foldit(K + G, R) :- foldit(K, K1), append([G], K1, L), my_sort(L, R).
%% foldit(K, [K]).
%% arrange(L, R) :- my_sort(L, R).

%% %% z12_2(K - (G), R) :- z12_2(K, K1), z12_2(G, G1), 
%% %%                          append_list([-G1], K1, List), my_sort(List, R).

%% %% z12_2(K+(G), R) :- z12_2(K, H), z12_2(G, G1), append([G1],  H, List), my_sort(List, R).
%% %% z12_2(K-(G), R) :- z12_2(K, H), z12_2(G, G1), append([-G1], H, List), my_sort(List, R).

%% %% z12_2(K-G, R) :- R = z12_2(K1-G1), K1 = z12_2(K, K1), G1 = z12_2(G, G1).

%% %% z12_2(N*x^P + F*x^P2, R) :- integer(F), integer(N), integer(P), integer(P2),
%% %%                             R = N*x^P + F*x^P2. %повернути двочлен

%% %% z12_2(N*x^P - F*x^P2, R) :- integer(F), integer(N),  integer(P), integer(P2),
%% %%                             R = N*x^P - F*x^P2. %повернути двочлен


%% is_monomial(K*x^N) :- integer(N), integer(K).
%% z12_2(K, G, R) :- z12_2(K + G, R). %to use in foldl
