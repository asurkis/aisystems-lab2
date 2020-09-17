distance(A, B, W) :- distance1(A, B, W).
distance(A, B, W) :- distance1(B, A, W).

indent(0).
indent(I) :-
  write('  '),
  succ(I1, I),
  indent(I1).

pathsum([_|[]], 0).
pathsum([A|[B|P]], S) :-
  distance(A, B, Sd),
  pathsum([B|P], S1),
  plus(S1, Sd, S).

dfs(A, [A|_], [A]).
dfs(B, [A|V], [A|P]) :-
  distance(A, C, _),
  \+ member(C, [A|V]),
  length([A|V], I),
  indent(I),
  write(C), nl,
  dfs(B, [C|[A|V]], P).

dfs(A, B, P, S) :-
  write(A), nl,
  dfs(B, [A], P), !,
  pathsum(P, S).

consed(A, B, [B|A]).
bfs(A, [[A|V]|_], P) :- reverse([A|V], P).
bfs(B, [V|R], P) :-
  V = [A|_],
  findall(X, (distance(A, X, _), \+ member(X, V)), T),
  maplist(consed(V), T, V1),
  append(R, V1, Q1),
  bfs(B, Q1, P).
bfs(A, B, P, S) :-
  bfs(B, [[A]], P),
  pathsum(P, S).

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
my_variant(A, B) :- my_variant_number(V), variant(V, A, B).

my_dfs(P, S) :-
  my_variant(A, B),
  dfs(A, B, P, S).

