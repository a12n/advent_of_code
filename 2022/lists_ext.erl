-module(lists_ext).

-include_lib("eunit/include/eunit.hrl").

-export([reduce/2, transpose/1]).

-spec reduce(fun((term(), term()) -> term()), nonempty_list()) -> term().
reduce(F, [Head | Tail]) -> lists:foldl(F, Head, Tail).

-spec transpose(nonempty_list(nonempty_list(term()))) -> nonempty_list(nonempty_list(term())).
transpose([[] | _]) ->
    [];
transpose(Lists) ->
    Heads = [H || [H | _] <- Lists],
    Tails = [T || [_ | T] <- Lists],
    [Heads | transpose(Tails)].

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

-endif.
