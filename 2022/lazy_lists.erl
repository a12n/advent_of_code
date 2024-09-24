-module(lazy_lists).

-type lazy_list(Elt) :: list(Elt) | maybe_improper_list(Elt, fun(() -> lazy_list(Elt))).
-type lazy_list() :: lazy_list(term()).
-export_type([lazy_list/0, lazy_list/1]).

-define(LAZY(Expr), fun() -> Expr end).
-define(FORCE(Tail),
    if
        is_function(Tail, 0) -> Tail();
        is_list(Tail) -> Tail
    end
).

-export([
    from_list/1,
    to_list/1,
    append/2,
    duplicate/2,
    cycle/2,
    filter/2,
    map/2,
    filtermap/2,
    fold/3,
    foreach/2,
    seq/1, seq/2, seq/3,
    take/2,
    drop/2
]).

-spec from_list(list()) -> lazy_list().
from_list([]) -> [];
from_list([Elt | Tail]) -> [Elt | ?LAZY(from_list(?FORCE(Tail)))].

-spec to_list(lazy_list()) -> list().
to_list([]) -> [];
to_list([Elt | Tail]) -> [Elt | to_list(?FORCE(Tail))].

-spec append(lazy_list(), lazy_list()) -> lazy_list().
append(LazyList1, []) -> LazyList1;
append([], LazyList2) -> LazyList2;
append([Elt1 | Tail1], LazyList2) -> [Elt1 | ?LAZY(append(?FORCE(Tail1), LazyList2))].

-spec duplicate(non_neg_integer() | infinity, term()) -> lazy_list().
duplicate(infinity, Elt) -> [Elt | ?LAZY(duplicate(infinity, Elt))];
duplicate(0, _) -> [];
duplicate(N, Elt) when N > 0 -> [Elt | ?LAZY(duplicate(N - 1, Elt))].

-spec cycle(non_neg_integer() | infinity, lazy_list()) -> lazy_list().
cycle(0, _) ->
    [];
cycle(1, LazyList) ->
    LazyList;
cycle(_, []) ->
    [];
cycle(infinity, LazyList) ->
    (fun
        Loop([]) -> Loop(LazyList);
        Loop([Elt | Tail]) -> [Elt | ?LAZY(Loop(?FORCE(Tail)))]
    end)(
        LazyList
    );
cycle(N, LazyList) when N > 0 ->
    (fun
        Loop(0, _) -> [];
        Loop(M, []) -> Loop(M - 1, LazyList);
        Loop(M, [Elt | Tail]) -> [Elt | ?LAZY(Loop(M, ?FORCE(Tail)))]
    end)(
        N, LazyList
    ).

-spec filter(fun((term()) -> boolean()), lazy_list()) -> lazy_list().
filter(Pred, LazyList) -> filtermap(fun(Elt) -> {Pred(Elt), Elt} end, LazyList).

-spec map(fun((term()) -> term()), lazy_list()) -> lazy_list().
map(Fun, LazyList) -> filtermap(fun(Elt) -> {true, Fun(Elt)} end, LazyList).

-spec filtermap(fun((term()) -> {true | false, term()} | boolean()), lazy_list()) -> lazy_list().
filtermap(_, []) ->
    [];
filtermap(Fun, [Elt | Tail]) ->
    case Fun(Elt) of
        true -> [Elt | ?LAZY(filtermap(Fun, ?FORCE(Tail)))];
        {true, Elt2} -> [Elt2 | ?LAZY(filtermap(Fun, ?FORCE(Tail)))];
        false -> filtermap(Fun, ?FORCE(Tail));
        {false, _} -> filtermap(Fun, ?FORCE(Tail))
    end.

-spec fold(fun(), term(), lazy_list()) -> term().
fold(_, Acc, []) -> Acc;
fold(Fun, Acc, [Elt | Tail]) -> fold(Fun, Fun(Elt, Acc), ?FORCE(Tail)).

-spec foreach(fun((term()) -> term()), lazy_list()) -> ok.
foreach(_, []) ->
    ok;
foreach(Fun, [Elt | Tail]) ->
    Fun(Elt),
    foreach(Fun, ?FORCE(Tail)).

-spec seq(integer()) -> lazy_list(integer()).
seq(From) -> seq(From, infinity).

-spec seq(integer(), integer() | infinity) -> lazy_list(integer()).
seq(From, To) when From =< To -> seq(From, To, 1).

-spec seq(integer(), integer() | infinity, neg_integer() | pos_integer()) -> lazy_list(integer()).
seq(From, To, Incr) when is_integer(To), From > To, Incr > 0 -> [];
seq(From, To, Incr) when is_integer(To), From < To, Incr < 0 -> [];
seq(From, To, Incr) -> [From | ?LAZY(seq(From + Incr, To, Incr))].

-spec take(non_neg_integer(), lazy_list()) -> lazy_list().
take(0, _) -> [];
take(_, []) -> [];
take(N, [Elt | Tail]) when N > 0 -> [Elt | ?LAZY(take(N - 1, ?FORCE(Tail)))].

-spec drop(non_neg_integer(), lazy_list()) -> lazy_list().
drop(0, LazyList) -> LazyList;
drop(_, []) -> [];
drop(N, [_ | Tail]) when N > 0 -> drop(N - 1, ?FORCE(Tail)).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

from_list_test() ->
    [1 | F1] = from_list([1, 2, 3]),
    [2 | F2] = F1(),
    [3 | F3] = F2(),
    [] = F3().

to_list_test() ->
    ?assertEqual([], to_list([])),
    ?assertEqual([1, 2, 3], to_list(from_list([1, 2, 3]))).

append_test() ->
    ?assertEqual([1, 2, 3], to_list(append(from_list([1, 2, 3]), []))),
    ?assertEqual([4, 5, 6], to_list(append([], from_list([4, 5, 6])))),
    ?assertEqual([1, 2, 3, 4, 5, 6], to_list(append(from_list([1, 2, 3]), from_list([4, 5, 6])))).

duplicate_test() ->
    ?assertEqual([], to_list(duplicate(0, a))),
    ?assertEqual([a, a, a], to_list(duplicate(3, a))).

cycle_test() ->
    ?assertEqual([], to_list(cycle(0, from_list([1, 2, 3])))),
    ?assertEqual([], to_list(cycle(3, []))),
    ?assertEqual([1, 2, 3, 1, 2, 3, 1, 2, 3], to_list(cycle(3, from_list([1, 2, 3])))).

filter_test() ->
    IsEven = fun(N) -> N rem 2 == 0 end,
    ?assertEqual([], to_list(filter(IsEven, []))),
    ?assertEqual([], to_list(filter(IsEven, from_list([1, 3, 5])))),
    ?assertEqual([2, 4, 6], to_list(filter(IsEven, from_list([1, 2, 3, 4, 5, 6])))).

map_test() ->
    Sqr = fun(N) -> N * N end,
    ?assertEqual([], to_list(map(Sqr, []))),
    ?assertEqual([1, 4, 9, 16], to_list(map(Sqr, from_list([1, 2, 3, 4])))).

filtermap_test() ->
    ?assertEqual(
        [<<"2">>, <<"4">>, <<"6">>],
        to_list(
            filtermap(
                fun
                    (N) when N rem 2 == 0 -> {true, integer_to_binary(N)};
                    (_) -> false
                end,
                from_list([1, 2, 3, 4, 5, 6])
            )
        )
    ).

fold_test() ->
    AddToMap = fun(N, Ans) -> Ans#{N => []} end,
    ?assertEqual(#{}, fold(AddToMap, #{}, [])),
    ?assertEqual(#{1 => [], 2 => [], 3 => []}, fold(AddToMap, #{}, from_list([1, 2, 3]))).

foreach_test() ->
    IsEven = fun(N) -> N rem 2 == 0 end,
    Sqr = fun(N) -> N * N end,
    Self = self(),
    spawn(fun() ->
        foreach(fun(N) -> Self ! N end, map(Sqr, filter(IsEven, from_list([1, 2, 3, 4, 5, 6]))))
    end),
    ?assertEqual(
        4,
        receive
            N -> N
        end
    ),
    ?assertEqual(
        16,
        receive
            N -> N
        end
    ),
    ?assertEqual(
        36,
        receive
            N -> N
        end
    ),
    ?assertEqual(
        ok,
        receive
            N -> N
        after 1000 -> ok
        end
    ).

-endif.
