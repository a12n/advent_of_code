-module(day_12).

-export([main/1]).

-spec main(1..2) -> ok.
main(1) ->
    HeightMap0 = grids:from_lines(io_ext:read_lines(standard_io)),
    [StartPos] = grids:find_values($S, HeightMap0),
    [EndPos] = grids:find_values($E, HeightMap0),
    HeightMap = HeightMap0#{StartPos := $a, EndPos := $z},
    MapSize = grids:size(HeightMap),
    %% io:format([
    %%     ansi:erase(display),
    %%     ansi:cursor({position, {1, 1}}),
    %%     grids:to_iodata(
    %%         maps:map(
    %%             fun(_, C) -> ansi:attrs({fg, {gray, 23 * (C - $a) div 25}}, C) end,
    %%             HeightMap
    %%         ),
    %%         MapSize
    %%     )
    %% ]),
    io:format(<<"~b~n">>, [distance(HeightMap, MapSize, StartPos, EndPos)]).

-spec distance(
    grids:grid(integer()),
    {pos_integer(), pos_integer()},
    grids:pos(),
    grids:pos()
) ->
    non_neg_integer().
distance(HeightMap, MapSize, StartPos, EndPos) ->
    Loop = fun Loop(Queue, Seen) ->
        case gb_sets:take_smallest(Queue) of
            {{Dist, Pos}, _} when Pos == EndPos -> Dist;
            {{Dist, Pos}, Queue2} ->
                Height = maps:get(Pos, HeightMap),
                %% io:format([ansi:cursor({position, Pos}), ansi:attrs({fg, green}, Height)]),
                %% timer:sleep(25),
                AdjPosList = [
                    AdjPos
                 || AdjPos <- [
                        grids:add_pos(Pos, grids:dir_to_pos(Dir))
                     || Dir <- [up, left, right, down]
                    ],
                    grids:is_valid_pos(AdjPos, MapSize),
                    not gb_sets:is_member(AdjPos, Seen),
                    abs(maps:get(AdjPos, HeightMap) - Height) < 2
                ],
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
    end,
    Loop(gb_sets:singleton({0, StartPos}), gb_sets:empty()).
