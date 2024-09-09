-module(day_12).

-include_lib("eunit/include/eunit.hrl").

-export([main/1]).

-spec main(1..2) -> ok.
main(1) ->
    HeightMap = maps:map(
        fun
            (_, $S) -> $a - 1;
            (_, $E) -> $z + 1;
            (_, C) -> C
        end,
        grids:from_lines(io_ext:read_lines(standard_io))
    ),
    MapSize = grids:size(HeightMap),
    [StartPos] = grids:find_values($a - 1, HeightMap),
    [EndPos] = grids:find_values($z + 1, HeightMap),
    io:format(<<"~b~n">>, [distance(HeightMap, MapSize, StartPos, EndPos)]).

-spec distance(
    grids:grid(integer()),
    {pos_integer(), pos_integer()},
    grids:pos(),
    grids:pos()
) ->
    non_neg_integer().
distance(HeightMap, MapSize, StartPos, EndPos) ->
    ?debugVal({MapSize, StartPos, EndPos}),
    Loop = fun Loop(Queue, Seen) ->
        %% ?debugVal(gb_sets:to_list(Queue)),
        %% ?debugVal(gb_sets:to_list(Seen)),
        case gb_sets:take_smallest(Queue) of
            {{Dist, Pos}, _} when Pos == EndPos -> Dist;
            {{Dist, Pos}, Queue2} ->
                ?debugVal({Dist, Pos}),
                case gb_sets:is_member(Pos, Seen) of
                    true ->
                        Loop(Queue2, Seen);
                    false ->
                        Height = maps:get(Pos, HeightMap),
                        AdjPosList =
                            lists:filter(
                                fun(AdjPos) ->
                                    AdjHeight = maps:get(AdjPos, HeightMap),
                                    abs(AdjHeight - Height) < 2
                                end,
                                [
                                    AdjPos
                                 || AdjPos <- [
                                        grids:add_pos(Pos, grids:dir_to_pos(Dir))
                                     || Dir <- [up, left, right, down]
                                    ],
                                    grids:is_valid_pos(AdjPos, MapSize)
                                ]
                            ),
                        Loop(
                            lists:foldl(
                                fun(AdjPos, Queue) ->
                                    gb_sets:add({Dist + 1, AdjPos}, Queue)
                                end,
                                Queue2,
                                AdjPosList
                            ),
                            gb_sets:add(Pos, Seen)
                        )
                end
        end
    end,
    Loop(gb_sets:singleton({0, StartPos}), gb_sets:empty()).
