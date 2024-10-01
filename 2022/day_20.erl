-module(day_20).

-include_lib("eunit/include/eunit.hrl").

-export([main/1, index/2]).

-spec main(1..2) -> ok.
main(1) ->
    Numbers = lists:map(fun binary_to_integer/1, io_ext:read_lines(standard_io)),
    ?debugFmt("Numbers ~p", [Numbers]),
    Numbers2 =
        lists:foldl(
            fun(I, Treap) ->
                %% ?debugFmt("~p: treap ~p", [I, treaps:to_list(Treap)]),
                {value, Pos, _} = treaps:search(
                    fun
                        ({J, _}) -> J == I;
                        (_) -> false
                    end,
                    Treap
                ),
                %% ?debugFmt("~p: found at pos ~p", [I, Pos]),
                {Value = {_, Number}, Pri, Treap2} = treaps:take(Pos, Treap),
                %% ?debugFmt("~p: taken ~p at pos ~p, after take ~p", [
                %%     I, Number, Pos, treaps:to_list(Treap2)
                %% ]),
                Pos2 = index(treaps:size(Treap2), (Pos - 1) + Number) + 1,
                %% ?debugFmt("~p: new pos ~p", [I, Pos2]),
                Treap3 = treaps:insert(Pos2, Value, Pri, Treap2),
                %% ?debugFmt("~p: after insert ~p", [
                %%     I, treaps:to_list(Treap3)
                %% ]),
                Treap3
            end,
            treaps:from_list(lists:enumerate(Numbers)),
            lists:seq(1, length(Numbers))
        ),
    ?debugFmt("Numbers2 ~p", [treaps:to_list(Numbers2)]),
    {value, ZeroPos, _} = treaps:search(
        fun
            ({_, 0}) -> true;
            (_) -> false
        end,
        Numbers2
    ),
    ?debugFmt("ZeroPos ~p", [ZeroPos]),
    Sum = lists:sum(
        lists:map(
            fun(Offset) ->
                Pos = index(treaps:size(Numbers2), (ZeroPos - 1) + Offset) + 1,
                {_, Value} = treaps:nth(Pos, Numbers2),
                ?debugFmt("Number at offset ~p from zero (pos ~p) is ~p", [Offset, Pos, Value]),
                Value
            end,
            [1000, 2000, 3000]
        )
    ),
    io:format(<<"~b~n">>, [Sum]).

-spec index(pos_integer(), integer()) -> non_neg_integer().
index(N, I) when I < 0 -> N + (I rem N);
index(N, I) -> I rem N.
