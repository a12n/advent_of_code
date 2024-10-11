%% Grove Positioning System
-module(day_20).

-include_lib("eunit/include/eunit.hrl").

-export([main/1]).

-spec main(1..2) -> ok.
main(Part) ->
    Numbers = treaps:from_list(
        %% Pairs of {Index, Number} values.
        lists:enumerate(
            lists:map(fun binary_to_integer/1, io_ext:read_lines(standard_io))
        )
    ),
    Numbers2 =
        case Part of
            1 -> mix(Numbers, 1);
            2 -> mix(treaps:map(fun({I, Number}) -> {I, Number * 811589153} end, Numbers), 10)
        end,
    {value, ZeroPos, _} = treaps:search(
        fun
            ({_, 0}) -> true;
            (_) -> false
        end,
        Numbers2
    ),
    io:format(standard_error, <<"[~p] = 0~n">>, [ZeroPos]),
    Sum = lists:sum(
        lists:map(
            fun(Offset) ->
                Pos = indices:remap((ZeroPos - 1) + Offset, treaps:size(Numbers2)) + 1,
                {_, Value} = treaps:nth(Pos, Numbers2),
                io:format(standard_error, <<"[~p + ~p; ~p] = ~p~n">>, [ZeroPos, Offset, Pos, Value]),
                Value
            end,
            [1000, 2000, 3000]
        )
    ),
    io:format(<<"~b~n">>, [Sum]).

-spec mix(treaps:treap(), non_neg_integer()) -> treaps:treap().
mix(Numbers, 0) ->
    Numbers;
mix(Numbers, N) ->
    Numbers2 =
        lists:foldl(
            fun(I, Treap) ->
                {value, Pos, _} = treaps:search(fun({J, _}) -> J == I end, Treap),
                {Value = {_, Number}, Pri, Treap2} = treaps:take(Pos, Treap),
                Pos2 = indices:remap((Pos - 1) + Number, treaps:size(Treap2)) + 1,
                treaps:insert(Pos2, Value, Pri, Treap2)
            end,
            Numbers,
            lists:seq(1, treaps:size(Numbers))
        ),
    mix(Numbers2, N - 1).
