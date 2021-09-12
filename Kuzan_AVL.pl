% 1 - Послідовності вузлів при обході AVL-дерева	
% z14_1(+Tree, -List) - послідовність вузлів, серединний обхід(inorder).
% z14_1(+Tree, +List) - чи правда що це послідовність вузлів цього дерева при обході праворуч.
z14_1(nil,[]).
z14_1(avl(Left, Root, Right), List) :- z14_1(Left, ListL), 
                                       z14_1(Right, ListR),
                                       append(ListL, [Root|ListR], List).

% 2 - Пошуку заданого елемента в AVL-дереві
% z14_2(+Tree, +E) - чи правда, що елемент Е можна знайти у даному дереві
% z14_2(+Tree, -E) - вивести всі значення вузлів дерева
z14_2(Tree, E) :- z14_1(Tree, List), member(E, List).

% 3 - Перевірки чи є заданий об'єкт AVL-деревом
% z14_3(+Tree) - чи є заданий об'єкт AVL-деревом
z14_3(T) :- once(checkAVL1(T)).

%checkAVL1(+Tree) - чи є корінь більший за всі значення у лівому піддереві та менший за всі значення у правому
checkAVL1(nil).
checkAVL1(avl(Left, Root, Right)) :- 
                                 z14_1(Left, LList),
                                 z14_1(Right, RList),
                                 checkL2(LList, Root),
                                 checkR2(RList, Root),
                                 checkAVL1(Left),
                                 checkAVL1(Right),
                                 height(avl(Left, Root, Right), _).

% height(+Tree, -Height) - отримати висоту AVL дерева
% height(+Tree, +Height) - перевірити, чи це справді висота AVL дерева
height(nil, 0).
height(avl(Left, _, Right), H) :- height(Left, HL),
                                  height(Right, HR),
                                  (HL is HR; HL is HR + 1; HL is HR - 1), % +- 1 різниця висот
                                  H is max(HL, HR) + 1.
% аналогічні до бінарних
checkR2([], _).
checkR2(RList, Root) :- sort(RList, [H|_]), Root < H.
checkL2([], _).
checkL2(LList, Root) :- sort(LList, LList2), last1(H, LList2), Root > H.

% отримати останній елемент списку
last1(L, [L]).
last1(E, [_|L]) :- last1(E, L).



% ТЕСТУВАННЯ
% ?- z14_1(avl(avl(avl(avl(nil,4,nil),5,avl(nil,6,nil)),10,avl(nil,11,nil)), 13, avl(avl(nil,14,nil),15,avl(nil,16,nil))), List).
% List = [4, 5, 6, 10, 11, 13, 14, 15, 16].
% ?- z14_2(avl(avl(avl(avl(nil,4,nil),5,avl(nil,6,nil)),10,avl(nil,11,nil)), 13, avl(avl(nil,14,nil),15,avl(nil,16,nil))), 5).
% true ;
% false.
% ?- z14_2(avl(avl(avl(avl(nil,4,nil),5,avl(nil,6,nil)),10,avl(nil,11,nil)), 13, avl(avl(nil,14,nil),15,avl(nil,16,nil))), 45).
% false.
% ?- z14_3(avl(avl(avl(avl(nil,4,nil),5,avl(nil,6,nil)),10,avl(nil,11,nil)), 13, avl(avl(nil,14,nil),15,avl(nil,16,nil)))).
% true.
% ?- z14_3(avl(avl(avl(nil, 1, nil), 2, avl(avl(nil, 3, nil), 4, avl(nil, 5, nil))), 6, avl(nil, 7, avl(avl(nil, 8, nil), 9, nil)))).
% false.
% avl(avl(avl(avl(nil,4,nil),5,avl(nil,6,nil)),10,avl(nil,11,nil)), 13, avl(avl(nil,14,nil),15,avl(nil,16,nil)))

%   тестове AVL-дерево
%
%              13
%
%         10        15
%
%      5     11  14     16
%
%   4    6