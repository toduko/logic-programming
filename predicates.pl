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
infix(InfL, L) :-
    append(_, X, L), append(InfL, _, X).

% ДОКУМЕНТАЦИЯ:
% insert(+L, X, M), insert(L, X, +M):
% M може да се получи чрез вмъкване
% на X някъде в L.

% ИМПЛЕМЕНТАЦИЯ:
insert(L, X, [X|L]).
insert([Y|L], X, [Y|M]) :- insert(L, X, M).

% ИМПЛЕМЕНТАЦИЯ 2: (втория тип извикване не работи)
insert2(L, X, M) :-
    append(A, B, L), append(A, [X|B], M).

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
reverse([X|L], RevXL) :-
    reverse(L, RevL), append(RevL, [X], RevXL).

% ИМПЛЕМЕНТАЦИЯ 2:
reverse2(L, RL) :- reverse_helper(L, RL, []).

% reverse_helper(+L, RL, +Acc):
% RL се получава от L чрез обръщане
% на неговите елементи и слепване
% на Acc накрая.
reverse_helper([], RL, RL).
reverse_helper([X|L], RXL, Acc) :-
    reverse_helper(L, RXL, [X|Acc]).

% ДОКУМЕНТАЦИЯ:
% subsequence(+L, SL):
% SL е подсписък на L.

% ИМПЛЕМЕНТАЦИЯ:
subsequence([], []).
subsequence([_|L], M) :- subsequence(L, M).
subsequence([X|L], [X|M]) :- subsequence(L, M).

% ДОКУМЕНТАЦИЯ:
% to_unique(+L, M):
% M се получава от L чрез премахване 
% на всички повторения.

% ИМПЛЕМЕНТАЦИЯ:
to_unique([], []).
to_unique([X|L], M) :-
    to_unique(L, M), member(X, M).
to_unique([X|L], [X|M]) :-
    to_unique(L, M), not(member(X, M)).

% ДОКУМЕНТАЦИЯ:
% seq_with_len(+Dom, +N, L):
% L е списък с дължина N, който 
% съдържа елементи само от Dom.

% ИМПЛЕМЕНТАЦИЯ:
seq_with_len(_, 0, []).
seq_with_len(Dom, N, [X|L]) :-
    N > 0, NMinus1 is N - 1,
    member(X, Dom),
    seq_with_len(Dom, NMinus1, L).

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
subset(X, Y) :-
    subsequence(Y, X1),
    to_unique(X1, X).

% ДОКУМЕНТАЦИЯ:
% powerset(+X, -PX):
% мн(PX) е степенното множество на мн(X).

% ИМПЛЕМЕНТАЦИЯ:
powerset(X, PX) :-
    powerset_helper(X, PX1, []),
    to_unique(PX1, PX).

% ДОКУМЕНТАЦИЯ:
% powerset_helper(+X, -PX, +Acc):
% мн(PX) е степенното множество на мн(X),
% обединено с мн(Acc).

% ИМПЛЕМЕНТАЦИЯ:
powerset_helper(X, PX, PX) :-
    not((
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
union(X, Y, XUY) :-
    append(X, Y, Z), to_unique(Z, XUY).

% ДОКУМЕНТАЦИЯ:
% difference(+X, +Y, -XMinusY):
% мн(XMinusY) е разликата на мн(X) и мн(Y).

% ИМПЛЕМЕНТАЦИЯ:
difference([], _, []).
difference([E|X], Y, Z) :-
    difference(X, Y, Z),
    (member(E, Y); member(E, X)).
difference([E|X], Y, [E|Z]) :-
    difference(X, Y, Z),
    not((member(E, X); member(E, Y))).

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
dom([(A, _)|RestR], [A|DomRestR]) :-
    dom(RestR, DomRestR).

% ДОКУМЕНТАЦИЯ:
% rng(+R, -RngR):
% мн(RngR) е областта от стойности на мн(R).

% ИМПЛЕМЕНТАЦИЯ:
rng(R, RngR) :-
    inverse(R, RInverse), dom(RInverse, RngR).

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
power_compose(R, 0, Id) :-
    fld(R, FldR), identity(FldR, Id).
power_compose(R, N, RToThePowerOfN) :-
    N > 0, NMinus1 is N - 1,
    power_compose(R, NMinus1, RToThePowerOfNMinus1),
    composition(RToThePowerOfNMinus1, R, RToThePowerOfN).

% ДОКУМЕНТАЦИЯ:
% identity(X, IdX):
% мн(IdX) е идентитетът върху мн(X).

% ИМПЛЕМЕНТАЦИЯ:
identity([], []).
identity([EX|RestX], [(EX, EX)|IdRestX]) :-
    identity(RestX, IdRestX).

% ДОКУМЕНТАЦИЯ:
% accumulate_power_compose(+R, +N, -X):
% мн(X) е обединението от K-кратната 
% композиция на мн(R) със себе си за 
% К между 0 и N.

% ИМПЛЕМЕНТАЦИЯ:
accumulate_power_compose(R, 0, Id) :-
    power_compose(R, 0, Id).
accumulate_power_compose(R, N, X) :-
    N > 0, NMinus1 is N - 1,
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                БЕЗКРАЙНИ ГЕНЕРАТОРИ
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ДОКУМЕНТАЦИЯ:
% natural(X):
% X е терм, който се получава 
% единствено от константата 0 
% и/или функционалния символ s.

% ИМПЛЕМЕНТАЦИЯ:
natural(0).
natural(s(N)) :- natural(N).

% ДОКУМЕНТАЦИЯ:
% sum(+N, +M, K):
% К e сбора на N и M (в горната 
% ни кодировка на естествени 
% числа).

% ИМПЛЕМЕНТАЦИЯ:
sum(N, 0, N).
sum(N, s(M), s(K)) :- sum(N, M, K).

% ДОКУМЕНТАЦИЯ:
% product(+N, +M, K):
% К e произведението на N и M 
% (в горната ни кодировка на 
% естествени числа).

% ИМПЛЕМЕНТАЦИЯ:
product(_, 0, 0).
product(N, s(M), K) :-
    product(N, M, K1), sum(K1, N, K).

% ДОКУМЕНТАЦИЯ:
% nat(N):
% N е естествено число.

% ИМПЛЕМЕНТАЦИЯ:
nat(0).
nat(N) :- nat(M), N is M + 1.

% ДОКУМЕНТАЦИЯ:
% pair(A, B):
% A и B са естествени числа.

% НАБЛЮДЕНИЕ:
% Всяка двойка N, M естествени имат сума
% всяко естествено K може да се разбие на 
% сума на две естествени по краен брой начини.

% ИМПЛЕМЕНТАЦИЯ:
pair(A, B) :- nat(N), sum_partition(N, A, B).

% ГРЕШНА ИДЕЯ:
% pair(A, B) :- nat(A), nat(B).
% Тук правим такова обхождане:
% NxN  0   1   2   3   4   ...   n   ...
% 0    ----------------------------------> 
% 1    ----------------------------------> няма да стигнем до този ред
% 2    ---------------------------------->
% 3    ---------------------------------->
% 4    ---------------------------------->
% .                     .
% .                     .
% .                     .
% n    ---------------------------------->
% .                     .
% .                     .
% .                     .

% ДОКУМЕНТАЦИЯ:
% sum_partition(+N, A, B):
% сумата на естествените 
% числа A и B е N.

% ИМПЛЕМЕНТАЦИЯ:
sum_partition(N, A, B) :-
    between1(0, N, A), B is N - A.

% ДОКУМЕНТАЦИЯ:
% between1(+Low, +High, X);
% X попада между Low и High и
% X, Low и High са естествени.

% ИМПЛЕМЕНТАЦИЯ:
between1(A, B, A) :-
    A =< B.
between1(A, B, X) :-
    A < B, A1 is A + 1,
    between1(A1, B, X).

% ДОКУМЕНТАЦИЯ:
% triple(A, B, C):
% A, B и C са естествени числа.

% ИМПЛЕМЕНТАЦИЯ:
triple(A, B, C) :-
    nat(N),
    sum_partition(N, A, B, C).

% ДОКУМЕНТАЦИЯ:
% sum_partition(+N, A, B, C):
% сумата на естествените 
% числа A, B и C е N.

% ИМПЛЕМЕНТАЦИЯ:
sum_partition(N, A, B, C) :-
    sum_partition(N, A, BPlusC),
    sum_partition(BPlusC, B, C).

% ДОКУМЕНТАЦИЯ:
% genKS(+K, +S, L):
% L има дължина K и сбор S, 
% като S е естествено.

% НАБЛЮДЕНИЕ 1:
% Една крайна редица L от 
% естествени числа има:
% 1) дължина;
% 2) сума.
% НАБЛЮДЕНИЕ 2:
% За всеки две естествени
% N и M има крайно много 
% редици L, чиято дължина 
% е N и сума е M.

% ИМПЛЕМЕНТАЦИЯ:
genKS(0, 0, []).
genKS(K, S, [X|L]) :-
    K > 0, K1 is K - 1,
    between1(0, S, X), S1 is S - X,
    genKS(K1, S1, L).


% ДОКУМЕНТАЦИЯ:
% finite_sequence(L):
% L е краен списък от 
% естествени числа.

% ИМПЛЕМЕНТАЦИЯ:
finite_sequence(L) :- pair(K, S), genKS(K, S, L).

% Регулярни изрази в Пролог:
% БАЗА: празнадума, a, b и празномножество 
% са регулярни изрази.
% СТЪПКА: ако r1 и r2 са регулярни изрази,
% то тогава следните са регулярни изрази:
% звезда(r1), (r1 + r2), (r1 * r2).

% ДОКУМЕНТАЦИЯ:
% regex(R, +N):
% R е регулярен израз, в който
% участват най-много N операции.

% ИМПЛЕМЕНТАЦИЯ:
regex(R, 0) :-
    member(R, [a, b, празнадума, празномножество]).
regex(R1 + R2, N) :-
    N > 0, NMinus1 is N - 1,
    sum_partition(NMinus1, N1, N2),
    regex(R1, N1),
    regex(R2, N2).
regex(R1 * R2, N) :-
    N > 0, NMinus1 is N - 1,
    sum_partition(NMinus1, N1, N2),
    regex(R1, N1),
    regex(R2, N2).
regex(звезда(R), N) :-
    N > 0, NMinus1 is N - 1,
    regex(R, NMinus1).

% ПРИМЕР ЗА ИЗРАЗ И СЪОТВЕТНОТО МУ ДЪРВО:
% Израз: празномножество * ((a + b) * a)
% Дървовидно представяне:
%                     *
%                   /   \
%    празномножество     *
%                       / \
%                      +   a
%                     / \
%                    a   b

% ДОКУМЕНТАЦИЯ:
% regex(R):
% R е регулярен израз в Пролог.

% ИМПЛЕМЕНТАЦИЯ:
regex(R) :- nat(N), regex(R, N).

% Кумулативна йерархия: (до ниво omega)
% - V(0) е празното множество;
% - V(n+1) е степенното множество на Vn.
% Накрая:
% V(omega) = V(0) U V(1) U V(2) U V(3) U ... U V(n) U ...
% Примери:
% {}, {{}}, {{}, {{}}}, {{}, {{}}, {{}, {{}}}}

% ДОКУМЕНТАЦИЯ:
% member_cumulative(X):
% мн(X) е член на някое крайно ниво 
% в кумулативната йерархия.

% ИМПЛЕМЕНТАЦИЯ:
member_cumulative(X) :-
    nat(N), member_cumulative(N, X).

% ДОКУМЕНТАЦИЯ:
% member_cumulative(N, X):
% мн(X) се намира на N-то ниво 
% в кумулативната йерархия.

% ИМПЛЕМЕНТАЦИЯ:
member_cumulative(N, X) :-
    cumulative(N, Vn), member(X, Vn).

% ДОКУМЕНТАЦИЯ:
% cumulative(N, Vn):
% мн(Vn) е V(N), тоест N-тото ниво 
% в кумулативната йерархия.

% ИМПЛЕМЕНТАЦИЯ:
cumulative(0, []).
cumulative(N, Vn) :-
    N > 0, NMinus1 is N - 1,
    cumulative(NMinus1, Vnminus1),
    powerset(Vnminus1, Vn).

% ДОКУМЕНТАЦИЯ:
% sigma_star(+Sigma, L):
% L е редица с елементи от Sigma.

% ИМПЛЕМЕНТАЦИЯ:
sigma_star(_, []).
sigma_star(Sigma, [X|L]) :-
    sigma_star(Sigma, L), member(X, Sigma).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                       ГРАФИ
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ПРЕДСТАВЯНЕ НА ГРАФИ В ПРОЛОГ:
% Един граф G = (V, E) ще представяме като
% двойка (VR, ER), където VR, ER са представянията 
% съответно на V и на E, с разликата, че при 
% неориентирани няма да използваме [X, Y] за 
% двуелементни множества (ребра), а ще използваме 
% записа (X, Y), както е по конвенция. Отново с
% об(X) ще бележим истинския обект, който 
% представяме в Пролог чрез X.

% ВАЖНО:
% За предикатите в тази секция приемаме, че 
% се подава валидно представяне на граф. Това 
% е важно за коректност на документацията.

% ДОКУМЕНТАЦИЯ:
% vertex(+G, X):
% об(X) е връх в графа об(G).

% ИМПЛЕМЕНТАЦИЯ:
vertex((V, _), X) :- member(X, V).

% ДОКУМЕНТАЦИЯ:
% edge(+G, X, Y):
% (об(X), об(Y)) е ребро в графа об(G).

% ИМПЛЕМЕНТАЦИЯ:
edge((_, E), X, Y) :-
    member((X, Y), E);
    member((Y, X), E).

% ДОКУМЕНТАЦИЯ:
% directed_edge(+G, X, Y):
% (об(X), об(Y)) е ребро в ориентирания граф об(G).

% ИМПЛЕМЕНТАЦИЯ:
directed_edge((_, E), X, Y) :- member((X, Y), E).

% ДОКУМЕНТАЦИЯ:
% is_path(+G, +P):
% об(P) е път в графа об(G).

% ИМПЛЕМЕНТАЦИЯ:
is_path(G, P) :-
    not((
            adjacent(V1, V2, P),
            not(edge(G, V1, V2))
        )).

% ДОКУМЕНТАЦИЯ:
% is_directed_path(+G, +P):
% об(P) е път в ориентирания граф об(G).

% ИМПЛЕМЕНТАЦИЯ:
is_directed_path(G, P) :-
    not((
            adjacent(V1, V2, P),
            not(directed_edge(G, V1, V2))
        )).

% ДОКУМЕНТАЦИЯ:
% is_cycle(+G, +C):
% об(C) е (прост) цикъл в графа об(G).

% ИМПЛЕМЕНТАЦИЯ:
is_cycle(G, C) :-
    is_path(G, C),
    append([X|Y], [X], C), Y \= [].

% ДОКУМЕНТАЦИЯ:
% is_directed_cycle(+G, +C):
% об(C) е (прост) цикъл в ориентирания граф об(G).

% ИМПЛЕМЕНТАЦИЯ:
is_directed_cycle(G, C) :-
    is_directed_path(G, C),
    append([X|_], [X], C).

% ДОКУМЕНТАЦИЯ:
% simple_path(+G, P):
% об(P) е прост път в графа об(G).

% ИМПЛЕМЕНТАЦИЯ:
simple_path((V, E), P) :-
    perm(V, PV),
    subsequence(PV, P),
    is_path((V, E), P).

% ДОКУМЕНТАЦИЯ:
% directed_simple_path(+G, P):
% об(P) е прост път в ориентирания граф об(G).

% ИМПЛЕМЕНТАЦИЯ:
directed_simple_path((V, E), P) :-
    perm(V, PV),
    subsequence(PV, P),
    is_directed_path((V, E), P).

% ДОКУМЕНТАЦИЯ:
% cycle(+G, C):
% об(C) е (прост) цикъл в графа об(G).

% ИМПЛЕМЕНТАЦИЯ:
cycle((V, E), C) :-
    len([_|V], NPlus1),
    between(3, NPlus1, K),
    seq_with_len(V, K, C),
    is_cycle((V, E), C).

% ДОКУМЕНТАЦИЯ:
% directed_cycle(+G, C):
% об(C) е (прост) цикъл в ориентирания граф об(G).

% ИМПЛЕМЕНТАЦИЯ:
directed_cycle((V, E), C) :-
    len([_|V], NPlus1),
    between(2, NPlus1, K),
    seq_with_len(V, K, C),
    is_directed_cycle((V, E), C).

% ДОКУМЕНТАЦИЯ:
% is_cyclic(+G):
% об(G) е цикличен граф.

% ИМПЛЕМЕНТАЦИЯ:
is_cyclic(G) :- cycle(G, _).

% ДОКУМЕНТАЦИЯ:
% is_directed_cyclic(+G):
% об(G) е ориентиран цикличен граф.

% ИМПЛЕМЕНТАЦИЯ:
is_directed_cyclic(G) :- directed_cycle(G, _).

% ДОКУМЕНТАЦИЯ:
% path(+G, P):
% об(P) е път в графа об(G).

% ИМПЛЕМЕНТАЦИЯ:
path((V, E), P) :-
    edge((V, E), _, _),
    sigma_star(V, P),
    is_path((V, E), P).
path(G, P) :-
    not(edge(G, _, _)),
    simple_path(G, P).

% ДОКУМЕНТАЦИЯ:
% directed_path(+G, P):
% об(P) е път в ориентирания граф об(G).

% ИМПЛЕМЕНТАЦИЯ:
directed_path((V, E), P) :-
    is_directed_cyclic((V, E)),
    sigma_star(V, P),
    is_directed_path((V, E), P).
directed_path(G, P) :-
    not(is_directed_cyclic(G)),
    directed_simple_path(G, P).

% ДОКУМЕНТАЦИЯ:
% connected(+G):
% об(G) е свързан граф.

% ИМПЛЕМЕНТАЦИЯ:
connected(G) :-
    not((
            vertex(G, X),
            vertex(G, Y),
            not((
                    simple_path(G, P),
                    append([X|_], [Y], P)
                ))
        )).

% ДОКУМЕНТАЦИЯ:
% strongly_connected(+G):
% об(G) е силно свързан ориентиран граф.

% ИМПЛЕМЕНТАЦИЯ:
strongly_connected(G) :-
    not((
            vertex(G, X),
            vertex(G, Y),
            not((
                    directed_simple_path(G, P),
                    append([X|_], [Y], P)
                ))
        )).

% ДОКУМЕНТАЦИЯ:
% tree(+T):
% об(G) е дърво.

% ИМПЛЕМЕНТАЦИЯ:
tree(T) :- connected(T), not(is_cyclic(T)).

% ДОКУМЕНТАЦИЯ:
% dag(+G):
% об(G) e ориентиран ацикличен граф.

% ИМПЛЕМЕНТАЦИЯ:
dag(G) :- not(is_directed_cyclic(G)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                ЗАДАЧИ ОТ КОНТРОЛНИ
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

p(XXS) :- not(предпоследен(_, XXS)).
p(XXS) :-
    предпоследен(XS, XXS),
    not((
            not(четна_позиция(X, XS));
            елемент(XS1, XXS), предпоследен(X1, XS1), нод(X, X1, 1)
        )).

четна_позиция(Y, [_|[X|L]]) :- Y = X; четна_позиция(Y, L).
нечетна_позиция(X, L) :- четна_позиция(X, [_|L]).

предпоследен(X, [X, _]).
предпоследен(X, [_|L]) :- предпоследен(X, L).

елемент(X, [Y|L]) :- Y = X; елемент(X, L).

нод(A, 0, A).
нод(A, B, C) :- B \= 0, R is A mod B, нод(B, R, C).
