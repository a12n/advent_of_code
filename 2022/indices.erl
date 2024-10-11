-module(indices).

-export([remap/2, remap/3]).

-spec remap(pos_integer(), integer()) -> non_neg_integer().
remap(I, N) when I < 0 ->
    case I rem N of
        0 -> 0;
        R -> R + N
    end;
remap(I, N) ->
    I rem N.

-spec remap(integer(), integer(), integer()) -> integer().
remap(I, Min, Max) -> remap(I - Min, Max - Min + 1) + Min.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

remap_test() ->
    ?assertEqual(3, remap(-5, 4)),
    ?assertEqual(0, remap(-4, 4)),
    ?assertEqual(1, remap(-3, 4)),
    ?assertEqual(2, remap(-2, 4)),
    ?assertEqual(3, remap(-1, 4)),

    ?assertEqual(0, remap(0, 4)),
    ?assertEqual(1, remap(1, 4)),
    ?assertEqual(2, remap(2, 4)),
    ?assertEqual(3, remap(3, 4)),
    ?assertEqual(0, remap(4, 4)),
    ?assertEqual(1, remap(5, 4)),
    ?assertEqual(2, remap(6, 4)),

    ?assertEqual(4, remap(-8, -1, 4)),
    ?assertEqual(-1, remap(-7, -1, 4)),
    ?assertEqual(0, remap(-6, -1, 4)),
    ?assertEqual(1, remap(-5, -1, 4)),
    ?assertEqual(2, remap(-4, -1, 4)),
    ?assertEqual(3, remap(-3, -1, 4)),
    ?assertEqual(4, remap(-2, -1, 4)),
    ?assertEqual(-1, remap(-1, -1, 4)),
    ?assertEqual(0, remap(0, -1, 4)),
    ?assertEqual(1, remap(1, -1, 4)),
    ?assertEqual(2, remap(2, -1, 4)),
    ?assertEqual(3, remap(3, -1, 4)),
    ?assertEqual(4, remap(4, -1, 4)),
    ?assertEqual(-1, remap(5, -1, 4)),
    ?assertEqual(0, remap(6, -1, 4)),
    ?assertEqual(1, remap(7, -1, 4)).

-endif.
