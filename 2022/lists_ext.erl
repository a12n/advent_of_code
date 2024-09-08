-module(lists_ext).

-include_lib("eunit/include/eunit.hrl").

-export([reduce/2, transpose/1, rotate/2]).

-spec reduce(fun((term(), term()) -> term()), nonempty_list()) -> term().
reduce(F, [Head | Tail]) -> lists:foldl(F, Head, Tail).

%%--------------------------------------------------------------------
%% Matrix functions.
%%--------------------------------------------------------------------

-spec transpose(nonempty_list(nonempty_list(term()))) -> nonempty_list(nonempty_list(term())).
transpose([[] | _]) ->
    [];
transpose(Lists) ->
    Heads = [H || [H | _] <- Lists],
    Tails = [T || [_ | T] <- Lists],
    [Heads | transpose(Tails)].

-spec rotate(cw | ccw, nonempty_list(nonempty_list(term()))) ->
    nonempty_list(nonempty_list(term())).
rotate(cw, Lists) -> transpose(lists:reverse(Lists));
rotate(ccw, Lists) -> lists:reverse(transpose(Lists)).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

transpose_test() ->
    ?assertEqual(
        [
            [1, 4, 7],
            [2, 5, 8],
            [3, 6, 9]
        ],
        transpose([
            [1, 2, 3],
            [4, 5, 6],
            [7, 8, 9]
        ])
    ).

rotate_test() ->
    Lists = [
        [1, 2, 3],
        [4, 5, 6],
        [7, 8, 9]
    ],
    CW1 = rotate(cw, Lists),
    ?assertEqual(
        [
            [7, 4, 1],
            [8, 5, 2],
            [9, 6, 3]
        ],
        CW1
    ),
    CW2 = rotate(cw, CW1),
    ?assertEqual(
        [
            [9, 8, 7],
            [6, 5, 4],
            [3, 2, 1]
        ],
        CW2
    ),
    CW3 = rotate(cw, CW2),
    ?assertEqual(
        [
            [3, 6, 9],
            [2, 5, 8],
            [1, 4, 7]
        ],
        CW3
    ),
    CW4 = rotate(cw, CW3),
    ?assertEqual(
        Lists,
        CW4
    ).

-endif.
