distance(A, B, W) :- distance1(A, B, W).
distance(A, B, W) :- distance1(B, A, W).
estimated(A, B, W) :- estimated1(A, B, W).
estimated(A, B, W) :- estimated1(B, A, W).

pathsum([_], 0).
pathsum([A,B|P], S) :-
  distance(A, B, Sd),
  pathsum([B|P], S1),
  plus(S1, Sd, S).

consed(A, B, [B|A]).
bfs_(A, [[A|V]|_], _, P) :- reverse([A|V], P).
bfs_(B, [[A|Ta]|Q], V, P) :-
  findall(X, (distance(A, X, _),
    \+ member(X, V)), T),
  write(A),
  write(' -> '),
  write(T), nl,
  maplist(consed([A|Ta]), T, Q1),
  append(Q, Q1, Q2),
  append(V, T, V2),
  bfs_(B, Q2, V2, P).
bfs(A, B, P, S) :-
  bfs_(B, [[A]], [A], P), !,
  pathsum(P, S).

dfs_(A, [A|_], [A]).
dfs_(B, [A|V], [A|P]) :-
  distance(A, C, _),
  \+ member(C, [A|V]),
  write(A),
  write(' -> '),
  write(C), nl,
  dfs_(B, [C,A|V], P).

dfs(A, B, P, S) :-
  dfs_(B, [A], P), !,
  pathsum(P, S).

dfs_lim_(A, [A|_], [A], D) :- D > 0 .
dfs_lim_(B, [A|V], [A|P], D) :-
  D > 0,
  distance(A, C, _),
  \+ member(C, [A|V]),
  write(A),
  write(' -> '),
  write(C), nl,
  succ(D1, D),
  dfs_lim_(B, [C,A|V], P, D1).

dfs_lim(A, B, P, S, D) :-
  dfs_lim_(B, [A], P, D), !,
  pathsum(P, S).

dfs_iterative(A, B, P, S, D) :-
  dfs_lim(A, B, P, S, D).
dfs_iterative(A, B, P, S, D) :-
  succ(D, D1),
  dfs_iterative(A, B, P, S, D1).
dfs_iterative(A, B, P, S) :- dfs_iterative(A, B, P, S, 0), !.

bds(Qa, Qb, P) :-
  member([X|Va], Qa),
  member([X|Vb], Qb),
  reverse([X|Va], Var),
  append(Var, Vb, P).
bds([[A|Va]|Qa], [[B|Vb]|Qb], P) :-
  findall(X, (distance(A, X, _), \+ member(X, [A|Va])), Ta),
  findall(X, (distance(B, X, _), \+ member(X, [B|Vb])), Tb),
  write(A),
  write(' -> '),
  write(Ta), nl,
  write(B),
  write(' <- '),
  write(Tb), nl, nl,
  maplist(consed([A|Va]), Ta, Va1),
  maplist(consed([B|Vb]), Tb, Vb1),
  append(Qa, Va1, Qa1),
  append(Qb, Vb1, Qb1),
  bds(Qa1, Qb1, P).

bds(A, B, P, S) :- bds([[A]], [[B]], P), !, pathsum(P, S).

extract_min([E], E, []).
extract_min([[A|Ta],[B|Tb]|T], [C|Tc], [[B|Tb]|T1]) :-
  A =< B, extract_min([[A|Ta]|T], [C|Tc], T1).
extract_min([[A|Ta],[B|Tb]|T], [C|Tc], [[A|Ta]|T1]) :-
  B < A, extract_min([[B|Tb]|T], [C|Tc], T1).

gfs_(A, [A|_], [A]).
gfs_(B, [A|V], [A|P]) :-
  findall([X, C], (
    distance(A, C, _),
    estimated(B, C, X),
    \+ member(C, [A|V])), T),
  extract_min(T, [_, A1], _),
  estimated(B, A, Ea),
  estimated(B, A1, Ea1),
  write(A),
  write(' ('),
  write(Ea),
  write(') -> '),
  write(A1),
  write(' ('),
  write(Ea1),
  write(')\n'),
  gfs_(B, [A1,A|V], P).

gfs(A, B, P, S) :- gfs_(B, [A], P), !, pathsum(P, S).

astar_predecessor(P, [S, A], [S,[A|P]]).
astar_(A, [[_,[A|V]]|_], _, P, S) :-
  reverse([A|V], P),
  pathsum(P, S).
astar_(B, [[Sa,[A|V]]|Q], M, P, S) :-
  findall([X, C], (
    distance(A, C, Dc),
    \+ member(C, M),
    estimated(B, A, Ea),
    estimated(B, C, Ec),
    plus(Da, Ea, Sa),
    plus(Da, Dc, Sc),
    plus(Sc, Ec, X)), T),
  write(A),
  write(' ('),
  write(Sa),
  write(') -> '),
  write(T), nl,
  maplist(astar_predecessor([A|V]), T, Q1),
  append(Q, Q1, Q2),
  extract_min(Q2, [Sa1,[A1|V1]], Q3),
  astar_(B, [[Sa1,[A1|V1]]|Q3], [A1|M], P, S).
astar(A, B, P, S) :-
  estimated(A, B, Ea),
  astar_(B, [[Ea,[A]]], [A], P, S), !.

my_variant(A, B) :- my_variant_number(V), variant(V, A, B).
my_bfs(P, S) :- my_variant(A, B), bfs(A, B, P, S).
my_dfs(P, S) :- my_variant(A, B), dfs(A, B, P, S).
my_dfs_lim(P, S, D) :- my_variant(A, B), dfs_lim(A, B, P, S, D).
my_dfs_iterative(P, S) :- my_variant(A, B), dfs_iterative(A, B, P, S).
my_bds(P, S) :- my_variant(A, B), bds(A, B, P, S).
my_gfs(P, S) :- my_variant(A, B), gfs(A, B, P, S).
my_astar(P, S) :- my_variant(A, B), astar(A, B, P, S).

distance1( vilnius       , brest           ,  531 ).
distance1( vitebsk       , brest           ,  638 ).
distance1( vitebsk       , vilnius         ,  360 ).
distance1( voronezh      , vitebsk         ,  869 ).
distance1( voronezh      , volgograd       ,  581 ).
distance1( volgograd     , vitebsk         , 1455 ).
distance1( vitebsk       , nizhny_novgorod ,  911 ).
distance1( vilnius       , daugavpils      ,  211 ).
distance1( kaliningrad   , brest           ,  699 ).
distance1( kaliningrad   , vilnius         ,  333 ).
distance1( kaunas        , vilnius         ,  102 ).
distance1( kyiv          , vilnius         ,  734 ).
distance1( kyiv          , zhytomyr        ,  131 ).
distance1( zhytomyr      , donetsk         ,  863 ).
distance1( zhytomyr      , volgograd       , 1493 ).
distance1( kishinev      , kyiv            ,  467 ).
distance1( kishinev      , donetsk         ,  812 ).
distance1( st_petersburg , vitebsk         ,  602 ).
distance1( st_petersburg , kaliningrad     ,  739 ).
distance1( st_petersburg , riga            ,  641 ).
distance1( moscow        , kazan           ,  815 ).
distance1( moscow        , nizhny_novgorod ,  411 ).
distance1( moscow        , minsk           ,  690 ).
distance1( moscow        , donetsk         , 1084 ).
distance1( moscow        , st_petersburg   ,  664 ).
distance1( murmansk      , st_petersburg   , 1412 ).
distance1( murmansk      , minsk           , 2238 ).
distance1( oryol         , vitebsk         ,  522 ).
distance1( oryol         , donetsk         ,  709 ).
distance1( oryol         , moscow          ,  368 ).
distance1( odessa        , kyiv            ,  487 ).
distance1( riga          , kaunas          ,  267 ).
distance1( tallinn       , riga            ,  308 ).
distance1( kharkiv       , kyiv            ,  471 ).
distance1( kharkiv       , simferopol      ,  639 ).
distance1( yaroslavl     , voronezh        ,  739 ).
distance1( yaroslavl     , minsk           ,  940 ).
distance1( ufa           , kazan           ,  525 ).
distance1( ufa           , samara          ,  461 ).

estimated1( nizhny_novgorod , brest           , 1391 ).
estimated1( nizhny_novgorod , vilnius         , 1189 ).
estimated1( nizhny_novgorod , vitebsk         ,  863 ).
estimated1( nizhny_novgorod , volgograd       ,  848 ).
estimated1( nizhny_novgorod , voronezh        ,  606 ).
estimated1( nizhny_novgorod , daugavpils      , 1081 ).
estimated1( nizhny_novgorod , donetsk         , 1015 ).
estimated1( nizhny_novgorod , zhitomir        , 1218 ).
estimated1( nizhny_novgorod , kazan           ,  328 ).
estimated1( nizhny_novgorod , kaliningrad     , 1483 ).
estimated1( nizhny_novgorod , kaunas          , 1267 ).
estimated1( nizhny_novgorod , kyiv            , 1104 ).
estimated1( nizhny_novgorod , kishinev        , 1466 ).
estimated1( nizhny_novgorod , minsk           , 1077 ).
estimated1( nizhny_novgorod , moscow          ,  401 ).
estimated1( nizhny_novgorod , murmansk        , 1508 ).
estimated1( nizhny_novgorod , nizhny_novgorod ,    0 ).
estimated1( nizhny_novgorod , odessa          , 1425 ).
estimated1( nizhny_novgorod , oryol           ,  632 ).
estimated1( nizhny_novgorod , riga            , 1214 ).
estimated1( nizhny_novgorod , st_petersburg   ,  896 ).
estimated1( nizhny_novgorod , samara          ,  524 ).
estimated1( nizhny_novgorod , simferopol      , 1437 ).
estimated1( nizhny_novgorod , tallinn         , 1185 ).
estimated1( nizhny_novgorod , ufa             ,  771 ).
estimated1( nizhny_novgorod , kharkiv         ,  872 ).
estimated1( nizhny_novgorod , yaroslavl       ,  287 ).

variant(  1 , murmansk      , odessa          ).
variant(  2 , st_petersburg , zhitomir        ).
variant(  3 , samara        , yaroslavl       ).
variant(  4 , riga          , ufa             ).
variant(  5 , kazan         , tallinn         ).
variant(  6 , simferopol    , murmansk        ).
variant(  7 , riga          , odessa          ).
variant(  8 , vilnius       , odessa          ).
variant(  9 , brest         , kazan           ).
variant( 10 , kharkiv       , nizhny_novgorod ).

variant_number_formula(D, M, V) :-
  plus(D, M, S),
  divmod(S, 10, _, T),
  succ(T, V).

my_birthday(4, 5).
my_variant_number(V) :- my_birthday(D, M), variant_number_formula(D, M, V).

