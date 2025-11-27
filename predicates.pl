%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                 БАЗОВИ ПРИМЕРИ
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Сократ е човек.
man(socrates).
% За всяко X:
% ако X е човек,
% то X е смъртен.
mortal(X) :- man(X).

% ?- mortal(X).
% X = socrates.

% Иванчо е родител на Марийка.
parent(ivancho, mariika).
% Марийка е родител на Гошо.
parent(mariika, gosho).
% Гошое родител на Пешо1.
parent(gosho, pesho1).
% Пешо1 е родител на Пешо2.
parent(pesho1, pesho2).
% Пешо2 е родител на Тошо.
parent(pesho2, tosho).

% ?- parent(X, Y).
% X = ivancho, Y = mariika;
% X = mariika, Y = gosho;
% X = gosho, Y = pesho1;
% X = pesho1, Y = pesho2;
% X = pesho2, Y = tosho.

% От X има път до X
% в родословното дърво.
parent_star(X, X).
% Ако Z е родител на Y
% и от X до Z има път в
% родословното дърво, то
% от X има път до Y
% в родословното дърво.
parent_star(X, Y) :- parent(Z, Y), parent_star(X, Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                      СПИСЪЦИ
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ДОКУМЕНТАЦИЯ:
% member(X, +L):
% X е елемент на L.

% ИМПЛЕМЕНТАЦИЯ:
% X е в началото, или
member(X, [X|_]).                 % (1)
% X е някъде в опашката.
member(X, [_|L]) :- member(X, L). % (2)

% ПРИМЕРНА АРГУМЕНТАЦИЯ ЗА ФИНИТНОСТ:
% (повече такива няма да пиша тук, а
%  само ще ги коментираме устно :p)
% При подаден втори аргумент member
% винаги завършва, защото:
% - ако е непразен списък, тогава ще
%   направим едно от следните:
%   (1): генерираме първият му елемент,
%   (2): ще направим рекурсия, в която 
%        вторият аргумент става все
%        по-малък списък (което не може
%        да продължава завинаги);
% - в противен случай директно ще
%   върнем лъжа и приключваме.

% ПРИМЕРИ:
% ?- member(X, [1, 2, 3]).
% X = 1;
% X = 2;
% X = 3;
% false.

% ДОКУМЕНТАЦИЯ:
% len(+L, N):
% N е дължината на списъка L.

% ИМПЛЕМЕНТАЦИЯ:
len([], 0).
len([_|L], N) :- len(L, NMinus1), N is NMinus1 + 1.

% ДОКУМЕНТАЦИЯ:
% append(+L, +M, K), append(L, M, +K):
% K е конкатенацията на списъците L и M.

% ИМПЛЕМЕНТАЦИЯ:
append([], L, L).
append([X|L], M, [X|K]) :- append(L, M, K).

% ДОКУМЕНТАЦИЯ:
% prefix(PrefL, +L):
% PrefL е префикс на L.

% ИМПЛЕМЕНТАЦИЯ:
prefix(PrefL, L) :- append(PrefL, _, L).

% ДОКУМЕНТАЦИЯ:
% suffix(SuffL, +L):
% SuffL е суфикс на L.

% ИМПЛЕМЕНТАЦИЯ:
suffix(SuffL, L) :- append(_, SuffL, L).

% ДОКУМЕНТАЦИЯ:
% infix(InfL, +L):
% InfL е инфикс на L.

% ИМПЛЕМЕНТАЦИЯ:
infix(InfL, L) :- append(_, X, L), append(InfL, _, X).

% ДОКУМЕНТАЦИЯ:
% insert(+L, X, M), insert(L, X, +M):
% M може да се получи чрез вмъкване
% на X някъде в L.

% ИМПЛЕМЕНТАЦИЯ:
insert(L, X, [X|L]).
insert([Y|L], X, [Y|M]) :- insert(L, X, M).

% ИМПЛЕМЕНТАЦИЯ 2: (втория тип извикване не работи)
insert2(L, X, M) :- append(A, B, L), append(A, [X|B], M).

% ДОКУМЕНТАЦИЯ:
% perm(+L, P):
% P е пермутация на L.

% ИМПЛЕМЕНТАЦИЯ:
perm([], []).
perm([X|L], P) :- perm(L, P1), insert(P1, X, P).

% ДОКУМЕНТАЦИЯ:
% adjacent(X, Y, +L):
% X са съседи Y в L.

% ИМПЛЕМЕНТАЦИЯ:
adjacent(X, Y, L) :- append(_, [X, Y|_], L).

% ДОКУМЕНТАЦИЯ:
% is_sorted(+L):
% L е сортиран възходящо спрямо <.

% ИМПЛЕМЕНТАЦИЯ:
is_sorted(L) :- not((
                      adjacent(X, Y, L),
                      X > Y
                    )).

% ДОКУМЕНТАЦИЯ:
% sort2(+L, SL):
% SL се получава чрез сортиране
% на L спрямо <.

% ИМПЛЕМЕНТАЦИЯ:
sort2(L, SL) :- perm(L, SL), is_sorted(SL).

% ДОКУМЕНТАЦИЯ:
% reverse(+L, RL):
% RL се получава от L чрез обръщане
% на неговите елементи.

% ИМПЛЕМЕНТАЦИЯ:
reverse([], []).
reverse([X|L], RevXL) :- reverse(L, RevL), append(RevL, [X], RevXL).

% ИМПЛЕМЕНТАЦИЯ 2:
reverse2(L, RL) :- reverse_helper(L, RL, []).

% reverse_helper(+L, RL, +Acc):
% RL се получава от L чрез обръщане
% на неговите елементи и слепване
% на Acc накрая.
reverse_helper([], RL, RL).
reverse_helper([X|L], RXL, Acc) :- reverse_helper(L, RXL, [X|Acc]).

% ДОКУМЕНТАЦИЯ:
% subsequence(+L, SL):
% SL е подсписък на L.

% ИМПЛЕМЕНТАЦИЯ:
subsequence([], []).
subsequence([_|L], M) :- subsequence(L, M).
subsequence([X|L], [X|M]) :- subsequence(L, M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                     МНОЖЕСТВА
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ПРЕДСТАВЯНЕ НА МНОЖЕСТВА В ПРОЛОГ:
% множеството {x1, ..., xn} ще представяме в 
% Пролог като списъка [пр(x1), ..., пр(xn)],
% където пр(xi) е представянето в Пролог на
% обекта xi. Обратно, всеки списък в Пролог
% L представя едно единствено множество,
% което ще бележим с мн(L).

% ДОКУМЕНТАЦИЯ:
% subset(X, +Y):
% мн(X) е подмножество на мн(Y).

% ИМПЛЕМЕНТАЦИЯ:
subset(X, Y) :- subsequence(Y, X).

% ДОКУМЕНТАЦИЯ:
% powerset(+X, PX):
% мн(PX) е степенното множество на мн(X).

% ИМПЛЕМЕНТАЦИЯ:
powerset(X, PX) :- powerset_helper(X, PX, []).

% ДОКУМЕНТАЦИЯ:
% powerset_helper(+X, PX, +Acc):
% мн(PX) е степенното множество на мн(X),
% обединено с мн(Acc).

% ИМПЛЕМЕНТАЦИЯ:
powerset_helper(X, PX, PX) :- not((
                                    subset(SX, X),
                                    not(member(SX, PX))
                                  )).
powerset_helper(X, PX, Acc) :- subset(SX, X),
                               not(member(SX, Acc)),
                               powerset_helper(X, PX, [SX|Acc]).

% ДОКУМЕНТАЦИЯ:
% intersection(+X, +Y, XAndY):
% мн(XAndY) е сечението на мн(X) и мн(Y).

% ИМПЛЕМЕНТАЦИЯ:
intersection(X, Y, XAndY) :- subset(XAndY, X), not((
                                                      member(EY, Y),
                                                      not(member(EY, XAndY))
                                                   )).

% ДОКУМЕНТАЦИЯ:
% union(+X, +Y, XUY):
% мн(XUY) е обединението на мн(X) и мн(Y).

% ИМПЛЕМЕНТАЦИЯ:
union(X, Y, XUY) :- append(X, Y, XUY).

% ДОКУМЕНТАЦИЯ:
% difference(+X, +Y, XMinusY):
% мн(XMinusY) е разликата на мн(X) и мн(Y).

% ИМПЛЕМЕНТАЦИЯ:
difference([], _, []). 
difference([E|X], Y, Z) :- difference(X, Y, Z), member(E, Y).
difference([E|X], Y, [E|Z]) :- difference(X, Y, Z), not(member(E, Y)).
