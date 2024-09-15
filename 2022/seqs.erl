-module(seqs).

-type seq(Value) :: fun(() -> {Value, _Next :: seq(Value)} | undefined).
-export_type([seq/1]).

-export([
    empty/0,
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
    range/1,
    range/2,
    range/3
]).

-spec empty() -> seq(term()).
empty() -> fun() -> undefined end.

-spec from_list(list()) -> seq(term()).
from_list([]) -> empty();
from_list([Value | Tail]) -> fun() -> {Value, from_list(Tail)} end.

-spec to_list(seq(term())) -> list().
to_list(Seq) ->
    %% TODO: Tail-recursive?
    case Seq() of
        undefined -> [];
        {Value, Next} -> [Value | to_list(Next)]
    end.

-spec append(seq(term()), seq(term())) -> seq(term()).
append(Seq1, Seq2) ->
    case Seq1() of
        undefined -> Seq2;
        {Value, Next} -> fun() -> {Value, append(Next, Seq2)} end
    end.

-spec duplicate(non_neg_integer() | infinity, term()) -> seq(term()).
duplicate(infinity, Value) -> fun() -> {Value, duplicate(infinity, Value)} end;
duplicate(0, _) -> empty();
duplicate(N, Value) when N > 0 -> fun() -> {Value, duplicate(N - 1, Value)} end.

-spec cycle(non_neg_integer() | infinity, seq(term())) -> seq(term()).
cycle(0, _) ->
    empty();
cycle(1, Seq) ->
    Seq;
cycle(N, InitialSeq) ->
    case InitialSeq() of
        undefined ->
            empty();
        {_, _} ->
            (fun
                Loop(_, I) when I >= N ->
                    empty();
                Loop(Seq, I) ->
                    case Seq() of
                        undefined -> Loop(InitialSeq, I + 1);
                        {Value, Next} -> fun() -> {Value, Loop(Next, I)} end
                    end
            end)(
                InitialSeq, 0
            )
    end.

-spec filter(fun((term()) -> boolean()), seq(term())) -> seq(term()).
filter(Pred, Seq) -> filtermap(fun(Value) -> {Pred(Value), Value} end, Seq).

-spec map(fun((term()) -> term()), seq(term())) -> seq(term()).
map(Fun, Seq) -> filtermap(fun(Value) -> {true, Fun(Value)} end, Seq).

-spec filtermap(fun((term()) -> {true | false, term()} | boolean()), seq(term())) -> seq(term()).
filtermap(Fun, Seq) ->
    fun() ->
        case Seq() of
            undefined ->
                undefined;
            {Value, Next} ->
                case Fun(Value) of
                    false -> (filtermap(Fun, Next))();
                    {false, _} -> (filtermap(Fun, Next))();
                    true -> {Value, filtermap(Fun, Next)};
                    {true, Value2} -> {Value2, filtermap(Fun, Next)}
                end
        end
    end.

-spec fold(fun((term(), term()) -> term()), term(), seq(term())) -> term().
fold(Fun, Acc, Seq) ->
    case Seq() of
        undefined -> Acc;
        {Value, Next} -> fold(Fun, Fun(Value, Acc), Next)
    end.

-spec foreach(fun(), seq(term())) -> ok.
foreach(Fun, Seq) ->
    case Seq() of
        undefined ->
            ok;
        {Value, Next} ->
            Fun(Value),
            foreach(Fun, Next)
    end.

-spec range(integer()) -> seq(integer()).
range(From) -> range(From, infinity).

-spec range(integer(), integer() | infinity) -> seq(integer()).
range(From, To) when From =< To -> range(From, To, 1).

-spec range(integer(), integer() | infinity, neg_integer() | pos_integer()) -> seq(integer()).
range(From, infinity, Incr) -> fun() -> {From, range(From + Incr, infinity, Incr)} end;
range(From, To, Incr) when Incr > 0, From > To -> empty();
range(From, To, Incr) when Incr < 0, From < To -> empty();
range(From, To, Incr) -> fun() -> {From, range(From + Incr, To, Incr)} end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

empty_test() ->
    Seq = empty(),
    ?assertEqual(undefined, Seq()),
    ?assertEqual(undefined, Seq()).

from_list_test() ->
    Seq1 = from_list([]),
    ?assertEqual(undefined, Seq1()),
    Seq2 = from_list([1, 2, 3]),
    {1, Seq3} = Seq2(),
    {2, Seq4} = Seq3(),
    {3, Seq5} = Seq4(),
    undefined = Seq5().

to_list_test() ->
    ?assertEqual([], to_list(empty())),
    ?assertEqual([1, 2, 3], to_list(from_list([1, 2, 3]))).

append_test() ->
    ?assertEqual([1, 2, 3], to_list(append(from_list([1, 2, 3]), empty()))),
    ?assertEqual([4, 5, 6], to_list(append(empty(), from_list([4, 5, 6])))),
    ?assertEqual([1, 2, 3, 4, 5, 6], to_list(append(from_list([1, 2, 3]), from_list([4, 5, 6])))).

duplicate_test() ->
    ?assertEqual([], to_list(duplicate(0, a))),
    ?assertEqual([a, a, a], to_list(duplicate(3, a))).

cycle_test() ->
    ?assertEqual([], to_list(cycle(0, from_list([1, 2, 3])))),
    ?assertEqual([], to_list(cycle(3, empty()))),
    ?assertEqual([1, 2, 3, 1, 2, 3, 1, 2, 3], to_list(cycle(3, from_list([1, 2, 3])))).

filter_test() ->
    IsEven = fun(N) -> N rem 2 == 0 end,
    ?assertEqual([], to_list(filter(IsEven, empty()))),
    ?assertEqual([], to_list(filter(IsEven, from_list([1, 3, 5])))),
    ?assertEqual([2, 4, 6], to_list(filter(IsEven, from_list([1, 2, 3, 4, 5, 6])))).

map_test() ->
    Sqr = fun(N) -> N * N end,
    ?assertEqual([], to_list(map(Sqr, empty()))),
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
    ?assertEqual(#{}, fold(AddToMap, #{}, empty())),
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
