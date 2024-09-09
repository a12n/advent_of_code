-module(day_08).

-include_lib("eunit/include/eunit.hrl").

-export([main/1]).

-spec main(1..2) -> ok.
main(Part) ->
    HeightMap =
        maps:map(
            fun(_, Char) -> Char - $0 end,
            grids:from_lines(io_ext:read_lines(standard_io))
        ),
    HeightMapSize = grids:size(HeightMap),
    case Part of
        1 ->
            _Cache = ets:new(max_height, [set, private, named_table]),
            VisibleTrees = maps:filter(
                fun(Pos, Height) ->
                    Height > max_height(left, Pos, HeightMap, HeightMapSize) orelse
                        Height > max_height(right, Pos, HeightMap, HeightMapSize) orelse
                        Height > max_height(up, Pos, HeightMap, HeightMapSize) orelse
                        Height > max_height(down, Pos, HeightMap, HeightMapSize)
                end,
                HeightMap
            ),
            io:format(<<"~b~n">>, [maps:size(VisibleTrees)]);
        2 ->
            MaxScenicScore = maps:fold(
                fun(Pos, _, Max) ->
                    max(Max, scenic_score(Pos, HeightMap, HeightMapSize))
                end,
                -1,
                HeightMap
            ),
            io:format(<<"~b~n">>, [MaxScenicScore])
    end.

-spec max_height(grids:dir(), grids:pos(), grids:grid(integer()), {
    non_neg_integer(), non_neg_integer()
}) -> integer().
max_height(left, {_, Col}, _, _) when Col =< 1 -> -1;
max_height(up, {Row, _}, _, _) when Row =< 1 -> -1;
max_height(right, {_, Col}, _, {_, NumCols}) when Col >= NumCols -> -1;
max_height(down, {Row, _}, _, {NumRows, _}) when Row >= NumRows -> -1;
max_height(Dir, Pos, HeightGrid, GridSize) ->
    Key = {Dir, Pos},
    case ets:lookup(max_height, Key) of
        [{_, MaxHeight}] ->
            MaxHeight;
        [] ->
            Pos2 = grid_position:add(Pos, grid_position:from_direction(Dir)),
            MaxHeight = max(
                max_height(Dir, Pos2, HeightGrid, GridSize),
                maps:get(Pos2, HeightGrid)
            ),
            true = ets:insert(max_height, {Key, MaxHeight}),
            MaxHeight
    end.

-spec scenic_score(grids:pos(), grids:grid(integer()), {non_neg_integer(), non_neg_integer()}) ->
    pos_integer().
scenic_score(Pos, HeightGrid, GridSize) ->
    viewing_distance(left, Pos, HeightGrid, GridSize) *
        viewing_distance(right, Pos, HeightGrid, GridSize) *
        viewing_distance(up, Pos, HeightGrid, GridSize) *
        viewing_distance(down, Pos, HeightGrid, GridSize).

-spec viewing_distance(grids:dir(), grids:pos(), grids:grid(integer()), {
    non_neg_integer(), non_neg_integer()
}) -> non_neg_integer().
viewing_distance(Dir, Pos, HeightGrid, GridSize) ->
    viewing_distance(Dir, Pos, HeightGrid, GridSize, maps:get(Pos, HeightGrid)).

-spec viewing_distance(
    grids:dir(),
    grids:pos(),
    grids:grid(integer()),
    {non_neg_integer(), non_neg_integer()},
    non_neg_integer()
) -> non_neg_integer().
viewing_distance(left, {_, Col}, _, _, _) when Col =< 1 -> 0;
viewing_distance(right, {_, Col}, _, {_, NumCols}, _) when Col >= NumCols -> 0;
viewing_distance(up, {Row, _}, _, _, _) when Row =< 1 -> 0;
viewing_distance(down, {Row, _}, _, {NumRows, _}, _) when Row >= NumRows -> 0;
viewing_distance(Dir, Pos, HeightGrid, GridSize, Height) ->
    Pos2 = grid_position:add(Pos, grid_position:from_direction(Dir)),
    case maps:get(Pos2, HeightGrid) of
        Height2 when Height2 < Height ->
            1 + viewing_distance(Dir, Pos2, HeightGrid, GridSize, Height);
        _ ->
            1
    end.
