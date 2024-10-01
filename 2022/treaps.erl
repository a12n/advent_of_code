-module(treaps).

-record(treap_node, {
    value :: term(),
    left :: #treap_node{} | undefined,
    right :: #treap_node{} | undefined,
    priority :: integer(),
    size :: non_neg_integer()
}).

-type treap() :: #treap_node{} | undefined.

-export_type([treap/0]).

-export([empty/0, from_list/1, from_list/2, size/1, split/2, merge/2, search/2, foldl/3, foldr/3]).

-spec empty() -> treap().
empty() -> undefined.

-spec from_list(list()) -> treap().
from_list(List) ->
    lists:foldl(
        fun(Value, Root) ->
            merge(Root, #treap_node{
                value = Value,
                priority = rand:uniform(16#FFFFFFFF_FFFFFFFF),
                size = 1
            })
        end,
        empty(),
        List
    ).

-spec from_list(list(), rand:state()) -> {treap(), rand:state()}.
from_list(List, Rand0) ->
    lists:foldl(
        fun(Value, {Root, Rand}) ->
            {Pri, Rand2} = rand:uniform_s(16#FFFFFFFF_FFFFFFFF, Rand),
            Root2 = merge(Root, #treap_node{
                value = Value,
                priority = Pri,
                size = 1
            }),
            {Root2, Rand2}
        end,
        {empty(), Rand0},
        List
    ).

-spec size(treap()) -> non_neg_integer().
size(undefined) -> 0;
size(#treap_node{size = Size}) -> Size.

-spec split(non_neg_integer(), treap()) -> {treap(), treap()}.
split(_, undefined) ->
    {undefined, undefined};
split(0, Treap = #treap_node{}) ->
    {undefined, Treap};
split(N, Treap = #treap_node{size = Size}) when N == Size -> {Treap, undefined};
split(N, #treap_node{value = Value, left = Left, right = Right, priority = Pri}) ->
    case ?MODULE:size(Left) of
        LeftSize when N > LeftSize ->
            {Part1, Part2} = split(N - LeftSize - 1, Right),
            {
                update_size(
                    #treap_node{
                        value = Value,
                        left = Left,
                        right = Part1,
                        priority = Pri
                    }
                ),
                Part2
            };
        _LeftSize ->
            {Part1, Part2} = split(N, Left),
            {
                Part1,
                update_size(
                    #treap_node{
                        value = Value,
                        left = Part2,
                        right = Right,
                        priority = Pri
                    }
                )
            }
    end.

-spec merge(treap(), treap()) -> treap().
merge(undefined, Treap2) ->
    Treap2;
merge(Treap1, undefined) ->
    Treap1;
merge(
    Treap1 = #treap_node{priority = Pri1, right = Right1},
    Treap2 = #treap_node{priority = Pri2, left = Left2}
) ->
    update_size(
        case Pri1 > Pri2 of
            true -> Treap1#treap_node{right = merge(Right1, Treap2)};
            false -> Treap2#treap_node{left = merge(Treap1, Left2)}
        end
    ).

-spec search(fun((term()) -> boolean()), treap()) -> false | {value, non_neg_integer(), term()}.
search(_, undefined) ->
    false;
search(Pred, #treap_node{value = Value, left = Left, right = Right}) ->
    case search(Pred, Left) of
        {value, Index, Found} ->
            {value, Index, Found};
        false ->
            case Pred(Value) of
                true ->
                    {value, ?MODULE:size(Left), Value};
                false ->
                    case search(Pred, Right) of
                        {value, Index, Found} ->
                            {value, ?MODULE:size(Left) + 1 + Index, Found};
                        false ->
                            false
                    end
            end
    end.

-spec foldl(fun((term(), term()) -> term()), term(), treap()) -> term().
foldl(_, Acc, undefined) ->
    Acc;
foldl(Fun, Acc, #treap_node{value = Value, left = Left, right = Right}) ->
    foldl(Fun, Fun(Value, foldl(Fun, Acc, Left)), Right).

-spec foldr(fun((term(), term()) -> term()), term(), treap()) -> term().
foldr(_, Acc, undefined) ->
    Acc;
foldr(Fun, Acc, #treap_node{value = Value, left = Left, right = Right}) ->
    foldr(Fun, Fun(Value, foldr(Fun, Acc, Right)), Left).

-spec update_size(#treap_node{}) -> #treap_node{}.
update_size(Treap = #treap_node{left = Left, right = Right}) ->
    Treap#treap_node{size = 1 + ?MODULE:size(Left) + ?MODULE:size(Right)}.
