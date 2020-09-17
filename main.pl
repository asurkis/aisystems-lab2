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
