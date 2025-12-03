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
% (1) X е в началото, или
member(X, [X|_]).
% (2) X е някъде в опашката.
member(X, [_|L]) :- member(X, L).

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

% to_unique(+L, M):
% M се получава от L чрез премахване 
% на всички повторения.
to_unique([], []).
to_unique([X|L], M) :- to_unique(L, M), member(X, M).
to_unique([X|L], [X|M]) :- to_unique(L, M), not(member(X, M)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                     МНОЖЕСТВА
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ПРЕДСТАВЯНЕ НА МНОЖЕСТВА В ПРОЛОГ:
% множеството {x1, ..., xn} ще представяме в 
% Пролог като списъка [пр(x1), ..., пр(xn)],
% където пр(xi) е представянето в Пролог на
% обекта xi. Обратно, всеки списък в Пролог
% L представя едно единствено множество,
% което ще бележим с мн(L). В общия случай, 
% когато имаме представяне X на даден обект 
% в Пролог, обекта ще бележим с об(X).

% ДОКУМЕНТАЦИЯ:
% subset(-X, +Y):
% мн(X) е подмножество на мн(Y).

% ИМПЛЕМЕНТАЦИЯ:
subset(X, Y) :- subsequence(Y, X1), to_unique(X1, X).

% ДОКУМЕНТАЦИЯ:
% powerset(+X, -PX):
% мн(PX) е степенното множество на мн(X).

% ИМПЛЕМЕНТАЦИЯ:
powerset(X, PX) :-
    powerset_helper(X, PX1, []), to_unique(PX1, PX).

% ДОКУМЕНТАЦИЯ:
% powerset_helper(+X, -PX, +Acc):
% мн(PX) е степенното множество на мн(X),
% обединено с мн(Acc).

% ИМПЛЕМЕНТАЦИЯ:
powerset_helper(X, PX, PX) :- not((
                                      subset(SX, X),
                                      not(member(SX, PX))
                                  )).
powerset_helper(X, PX, Acc) :-
    subset(SX, X),
    not(member(SX, Acc)),
    powerset_helper(X, PX, [SX|Acc]).

% ДОКУМЕНТАЦИЯ:
% intersection(+X, +Y, -XAndY):
% мн(XAndY) е сечението на мн(X) и мн(Y).

% ИМПЛЕМЕНТАЦИЯ:
intersection(X, Y, XAndY) :-
    subset(Z, X),
    not((
            member(EY, Y),
            not(member(EY, Z))
        )),
  to_unique(Z, XAndY).

% ДОКУМЕНТАЦИЯ:
% union(+X, +Y, -XUY):
% мн(XUY) е обединението на мн(X) и мн(Y).

% ИМПЛЕМЕНТАЦИЯ:
union(X, Y, XUY) :- append(X, Y, Z), to_unique(Z, XUY).

% ДОКУМЕНТАЦИЯ:
% difference(+X, +Y, -XMinusY):
% мн(XMinusY) е разликата на мн(X) и мн(Y).

% ИМПЛЕМЕНТАЦИЯ:
difference([], _, []).
difference([E|X], Y, Z) :- difference(X, Y, Z), (member(E, Y); member(E, X)).
difference([E|X], Y, [E|Z]) :- difference(X, Y, Z), not((member(E, X); member(E, Y))).

% ДОКУМЕНТАЦИЯ:
% cartesian_product(+A, +B, -AxB):
% мн(AxB) е декартовото произведение
% на мн(A) и мн(B).

% ИМПЛЕМЕНТАЦИЯ:
cartesian_product([], _, []).
cartesian_product([EA|RestA], B, AxB) :-
    cartesian_product(RestA, B, RestAxB),
    singleton_product(EA, B, EAxB),
    union(EAxB, RestAxB, AxB).

% ДОКУМЕНТАЦИЯ:
% singleton_product(+X, +A, -SgXxA):
% мн(SgXxA) е декартовото произведение
% на { об(X) } и мн(A).

% ИМПЛЕМЕНТАЦИЯ:
singleton_product(_, [], []).
singleton_product(X, [EA|RestA], [(X, EA)|SgXxRestA]) :-
    singleton_product(X, RestA, SgXxRestA).

% ДОКУМЕНТАЦИЯ:
% is_relation(+R):
% мн(R) е релация.

% ИМПЛЕМЕНТАЦИЯ:
is_relation(R) :- not((member(X, R), X \= (_, _))).

% ДОКУМЕНТАЦИЯ:
% dom(+R, -DomR):
% мн(DomR) е домейнът на мн(R).

% ИМПЛЕМЕНТАЦИЯ:
dom([], []).
dom([(A, _)|RestR], [A|DomRestR]) :- dom(RestR, DomRestR).

% ДОКУМЕНТАЦИЯ:
% rng(+R, -RngR):
% мн(RngR) е областта от стойности на мн(R).

% ИМПЛЕМЕНТАЦИЯ:
rng(R, RngR) :- inverse(R, RInverse), dom(RInverse, RngR).

% ДОКУМЕНТАЦИЯ:
% inverse(+R, -RInverse):
% мн(RInverse) е обратната релация на мн(R).

% ИМПЛЕМЕНТАЦИЯ:
inverse([], []).
inverse([(X, Y)|RestR], [(Y, X)|RestRInverse]) :-
    inverse(RestR, RestRInverse).

% ДОКУМЕНТАЦИЯ:
% fld(+R, -FldR):
% мн(FldR) е полето на мн(R).

% ИМПЛЕМЕНТАЦИЯ:
fld(R, FldR) :-
    dom(R, DomR), rng(R, RngR), union(DomR, RngR, FldR).

% ДОКУМЕНТАЦИЯ:
% composition(+R, +S, -RCompS):
% мн(RCompS) е композицията на мн(R) и мн(S).

% ИМПЛЕМЕНТАЦИЯ:
composition([], _, []).
composition([EA|RestA], B, ACompB) :-
    composition(RestA, B, RestACompB),
    singleton_composition(EA, B, EACompB),
    union(EACompB, RestACompB, ACompB).

% ДОКУМЕНТАЦИЯ:
% singleton_composition(+X, +A, -SgXCompA):
% мн(SgXCompA) е композицията на { об(X) } и мн(A).

% ИМПЛЕМЕНТАЦИЯ:
singleton_composition(_, [], []).
singleton_composition((X, Y), [(Z, _)|RestA], SgXYCompRestA) :-
    singleton_composition((X, Y), RestA, SgXYCompRestA),
    Y \= Z.
singleton_composition((X, Y), [(Y, Z)|RestA], [(X, Z)|SgXYCompRestA]) :-
    singleton_composition((X, Y), RestA, SgXYCompRestA).

% ДОКУМЕНТАЦИЯ:
% power_compose(+R, +N, -RToThePowerOfN):
% мн(RToThePowerOfN) е N-кратната композиция 
% на мн(R) със себе си.

% ИМПЛЕМЕНТАЦИЯ:
power_compose(R, 0, Id) :- fld(R, FldR), identity(FldR, Id).
power_compose(R, N, RToThePowerOfN) :-
    N > 0, NMinus1 is N - 1,
    power_compose(R, NMinus1, RToThePowerOfNMinus1),
    composition(RToThePowerOfNMinus1, R, RToThePowerOfN).

% ДОКУМЕНТАЦИЯ:
% identity(X, IdX):
% мн(IdX) е идентитетът върху мн(X).

% ИМПЛЕМЕНТАЦИЯ:
identity([], []).
identity([EX|RestX], [(EX, EX)|IdRestX]) :- identity(RestX, IdRestX).

% ДОКУМЕНТАЦИЯ:
% accumulate_power_compose(+R, +N, -X):
% мн(X) е обединението от K-кратната 
% композиция на мн(R) със себе си за 
% К между 0 и N.

% ИМПЛЕМЕНТАЦИЯ:
accumulate_power_compose(R, 0, Id) :- power_compose(R, 0, Id).
accumulate_power_compose(R, N, X) :- N > 0, NMinus1 is N - 1,
    accumulate_power_compose(R, NMinus1, Y),
    power_compose(R, N, Z),
    union(Y, Z, X).

% ДОКУМЕНТАЦИЯ:
% trcl(+R, -TrclR):
% мн(TrclR) е транзитивното и рефлексивно 
% затваряне на мн(R).

% ИМПЛЕМЕНТАЦИЯ:
trcl(R, TrclR) :-
    fld(R, FR), len(FR, N),
    accumulate_power_compose(R, N, TrclR).
