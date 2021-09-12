%1 Напишіть предикат, який перетворює вихідний список у список позицій від'ємних елементів.

getNeg(List, Pos) :- findall(Id, indexOf(List, Id), Pos). %збираємо відповіді у список
%getNeg([1,0,-4,-44,], B).
%getNeg([], B).

indexOf([Element|_], 0) :- Element < 0. 
indexOf([_|Tail], Index):-
  indexOf(Tail, Index1), %рекурсія по хвосту
  Index is Index1+1.   % інкремент індексу

%2. Напишіть предикат, що замінює всі входження заданого елемента на символ change_done.
replaceAll([], _, []). %випадок коли список порожній
replaceAll([G|T1], G, [change_done|T2]) :- replaceAll(T1, G, T2). %замінюємо, коли знаходим
replaceAll([H|T1], G, [H|T2]) :- dif(H, G), replaceAll(T1, G, T2). %запевнюємось що голови списків різні, рекурсія по хвосту
%replaceAll([1,2,3,5], 5, X).

%3. Напишіть предикат, що перетворює будь-який список арабських чисел
rome_help(N, _):- N < 0, !, fail.
rome_help(0, []).
rome_help(N, ['I'|Roman]) :- N < 4, M is N - 1, rome_help(M, Roman).
rome_help(4, ['IV']).
rome_help(5, ['V']).
rome_help(N, ['V'|Roman]) :- N < 9, M is N - 5, rome_help(M, Roman).
rome_help(9, ['IX']).
rome_help(N, ['X'|Roman]) :- N < 40, M is N - 10, rome_help(M, Roman).
rome_help(N, ['XL'|Roman]) :- N < 50, M is N - 40, rome_help(M, Roman).
rome_help(N, ['L'|Roman]) :- N < 90, M is N - 50, rome_help(M, Roman).

roman(N, R) :- rome_help(N, L), atomic_list_concat(L, R).

roman(N) :- roman(N, R), write(R).
%roman(56, B).

rr(N, R) :- maplist(roman, N, R).

%4. Напишіть предикат, що здійsснює циклічний зсув елементів списку на один вправо.

init(L, Init) :- append(Init, [Z], L), last(L, Z).%init - prefix, last - suffix in append 
shift([],[]). %базовий випадок
shift(Xs, [Z|Init]) :- init(Xs, Init), last(Xs, Z). %беремо всі елементи крім останнього (init), вставляємо останній на початку
%shift([1,2,4,5], X).
%shift([], X).

%5. Напишіть предикат, що реалізує множення матриці (список списків) на вектор.

%правило для скалярного добуток
scalar_product([], [], 0).
scalar_product([H1|T1], [H2|T2], Res) :- 
  Prod is H1 * H2, %добуток голів
  scalar_product(T1, T2, Rest), %рекурсія по хвостах
  Res is Prod + Rest. %обчислити


z105(M, V, R) :- maplist(scalar_product(V), M, R).
%z105([[2, 4, 0],[-2, 1, 3],[-1, 0, 1]],[1, 2, -1], X).

%% insertAtEnd(X,[ ],[X]).
%% insertAtEnd(X,[H|T],[H|Z]) :- insertAtEnd(X,T,Z).    
