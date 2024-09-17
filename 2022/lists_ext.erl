-module(lists_ext).

-include_lib("eunit/include/eunit.hrl").

-export([
    consec_pairs/1,
    reduce/2,
    seq/2,
    product/1,
    enumerate_lists/1,
    transpose_lists/1,
    rotate_lists/2
]).

-spec consec_pairs([term()]) -> [{term(), term()}].
consec_pairs([]) -> [];
consec_pairs([Elt1, Elt2 | List]) -> [{Elt1, Elt2} | consec_pairs(List)].

-spec reduce(fun((term(), term()) -> term()), nonempty_list()) -> term().
reduce(F, [Head | Tail]) -> lists:foldl(F, Head, Tail).

-spec seq(integer(), integer()) -> [integer()].
seq(From, To) when From =< To -> lists:seq(From, To);
seq(From, To) -> lists:seq(From, To, -1).

-spec product([number()]) -> number().
product(Numbers) -> lists:foldl(fun erlang:'*'/2, 1, Numbers).

%%--------------------------------------------------------------------
%% Matrix functions.
%%--------------------------------------------------------------------

-spec enumerate_lists(nonempty_list(nonempty_list(term()))) ->
    nonempty_list(nonempty_list({{pos_integer(), pos_integer()}, term()})).
enumerate_lists(Lists) ->
    lists:map(
        fun({Row, List}) ->
            lists:map(
                fun({Col, Value}) ->
                    {{Row, Col}, Value}
                end,
                lists:enumerate(List)
            )
        end,
        lists:enumerate(Lists)
    ).

-spec transpose_lists(nonempty_list(nonempty_list(term()))) -> nonempty_list(nonempty_list(term())).
transpose_lists([[] | _]) ->
    [];
transpose_lists(Lists) ->
    Heads = [H || [H | _] <- Lists],
    Tails = [T || [_ | T] <- Lists],
    [Heads | transpose_lists(Tails)].

-spec rotate_lists(cw | ccw, nonempty_list(nonempty_list(term()))) ->
    nonempty_list(nonempty_list(term())).
rotate_lists(cw, Lists) -> transpose_lists(lists:reverse(Lists));
rotate_lists(ccw, Lists) -> lists:reverse(transpose_lists(Lists)).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

enumerate_lists_test() ->
    ?assertEqual(
        [
            [{{1, 1}, a}, {{1, 2}, b}, {{1, 3}, c}],
            [{{2, 1}, d}, {{2, 2}, e}, {{2, 3}, f}],
            [{{3, 1}, g}, {{3, 2}, h}, {{3, 3}, i}]
        ],
        enumerate_lists(
            [
                [a, b, c],
                [d, e, f],
                [g, h, i]
            ]
        )
    ).

transpose_lists_test() ->
    ?assertEqual(
        [
            [1, 4, 7],
            [2, 5, 8],
            [3, 6, 9]
        ],
        transpose_lists([
            [1, 2, 3],
            [4, 5, 6],
            [7, 8, 9]
        ])
    ).

rotate_lists_test() ->
    Lists = [
        [1, 2, 3],
        [4, 5, 6],
        [7, 8, 9]
    ],
    CW1 = rotate_lists(cw, Lists),
    ?assertEqual(
        [
            [7, 4, 1],
            [8, 5, 2],
            [9, 6, 3]
        ],
        CW1
    ),
    CW2 = rotate_lists(cw, CW1),
    ?assertEqual(
        [
            [9, 8, 7],
            [6, 5, 4],
            [3, 2, 1]
        ],
        CW2
    ),
    CW3 = rotate_lists(cw, CW2),
    ?assertEqual(
        [
            [3, 6, 9],
            [2, 5, 8],
            [1, 4, 7]
        ],
        CW3
    ),
    CW4 = rotate_lists(cw, CW3),
    ?assertEqual(
        Lists,
        CW4
    ).

-endif.
