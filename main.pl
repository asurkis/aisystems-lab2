distance( vilnius       , brest           ,  531 ).
distance( vitebsk       , brest           ,  638 ).
distance( vitebsk       , vilnius         ,  360 ).
distance( voronezh      , vitebsk         ,  869 ).
distance( voronezh      , volgograd       ,  581 ).
distance( volgograd     , vitebsk         , 1455 ).
distance( vitebsk       , nizhny_novgorod ,  911 ).
distance( vilnius       , daugavpils      ,  211 ).
distance( kaliningrad   , brest           ,  699 ).
distance( kaliningrad   , vilnius         ,  333 ).
distance( kaunas        , vilnius         ,  102 ).
distance( kyiv          , vilnius         ,  734 ).
distance( kyiv          , zhytomyr        ,  131 ).
distance( zhytomyr      , donetsk         ,  863 ).
distance( zhytomyr      , volgograd       , 1493 ).
distance( kishinev      , kyiv            ,  467 ).
distance( kishinev      , donetsk         ,  812 ).
distance( st_petersburg , vitebsk         ,  602 ).
distance( st_petersburg , kaliningrad     ,  739 ).
distance( st_petersburg , riga            ,  641 ).
distance( moscow        , kazan           ,  815 ).
distance( moscow        , nizhny_novgorod ,  411 ).
distance( moscow        , minsk           ,  690 ).
distance( moscow        , donetsk         , 1084 ).
distance( moscow        , st_petersburg   ,  664 ).
distance( murmansk      , st_petersburg   , 1412 ).
distance( murmansk      , minsk           , 2238 ).
distance( oryol         , vitebsk         ,  522 ).
distance( oryol         , donetsk         ,  709 ).
distance( oryol         , moscow          ,  368 ).
distance( odessa        , kyiv            ,  487 ).
distance( riga          , kaunas          ,  267 ).
distance( tallinn       , riga            ,  308 ).
distance( kharkiv       , kyiv            ,  471 ).
distance( kharkiv       , simferopol      ,  639 ).
distance( yaroslavl     , voronezh        ,  739 ).
distance( yaroslavl     , minsk           ,  940 ).
distance( ufa           , kazan           ,  525 ).
distance( ufa           , samara          ,  461 ).

/* Если задавать правило инверсии, 
   то будет бесконечный поиск */
distance( brest           , vilnius       , 531 ).
distance( brest           , vitebsk       , 638 ).
distance( vilnius         , vitebsk       , 360 ).
distance( vitebsk         , voronezh      , 869 ).
distance( volgograd       , voronezh      , 581 ).
distance( vitebsk         , volgograd     , 455 ).
distance( nizhny_novgorod , vitebsk       , 911 ).
distance( daugavpils      , vilnius       , 211 ).
distance( brest           , kaliningrad   , 699 ).
distance( vilnius         , kaliningrad   , 333 ).
distance( vilnius         , kaunas        , 102 ).
distance( vilnius         , kyiv          , 734 ).
distance( zhytomyr        , kyiv          , 131 ).
distance( donetsk         , zhytomyr      , 863 ).
distance( volgograd       , zhytomyr      , 493 ).
distance( kyiv            , kishinev      , 467 ).
distance( donetsk         , kishinev      , 812 ).
distance( vitebsk         , st_petersburg , 602 ).
distance( kaliningrad     , st_petersburg , 739 ).
distance( riga            , st_petersburg , 641 ).
distance( kazan           , moscow        , 815 ).
distance( nizhny_novgorod , moscow        , 411 ).
distance( minsk           , moscow        , 690 ).
distance( donetsk         , moscow        , 084 ).
distance( st_petersburg   , moscow        , 664 ).
distance( st_petersburg   , murmansk      , 412 ).
distance( minsk           , murmansk      , 238 ).
distance( vitebsk         , oryol         , 522 ).
distance( donetsk         , oryol         , 709 ).
distance( moscow          , oryol         , 368 ).
distance( kyiv            , odessa        , 487 ).
distance( kaunas          , riga          , 267 ).
distance( riga            , tallinn       , 308 ).
distance( kyiv            , kharkiv       , 471 ).
distance( simferopol      , kharkiv       , 639 ).
distance( voronezh        , yaroslavl     , 739 ).
distance( minsk           , yaroslavl     , 940 ).
distance( kazan           , ufa           , 525 ).
distance( samara          , ufa           , 461 ).

indent(0).
indent(I) :-
  write('  '),
  succ(I1, I),
  indent(I1).

dfs(A, A, V, [A], 0) :-
  length(V, I), 
  indent(I), 
  write('Found solution!\n').
dfs(A, B, V, [A|P], S) :-
  distance(A, C, Sd),
  not(member(C, V)),
  length(V, I),
  indent(I),
  write(C),
  write('\n'),
  dfs(C, B, [C|V], P, S1),
  plus(Sd, S1, S).
start_dfs(A, B, P, S) :-
  write(A),
  write('\n'),
  dfs(A, B, [A], P, S), !,
  write('\nPath is: '),
  write(P),
  write('\nSum is: '),
  write(S),
  length(P, L),
  write('\nLen is: '),
  write(L),
  write('\n').

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
  start_dfs(A, B, P, S).

