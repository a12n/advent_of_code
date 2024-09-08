-module(day_08).

-include_lib("eunit/include/eunit.hrl").

-export([main/1]).

-spec main(1..2) -> ok.
main(1) ->
    _Cache = ets:new(max_height, [set, private, named_table]),
    HeightMap =
        maps:map(
            fun(_, Char) -> Char - $0 end,
            map_grids:from_lines(io_ext:read_lines(standard_io))
        ),
    HeightMapSize = map_grids:size(HeightMap),
    VisibleTrees =
        maps:filter(
            fun(Pos, Height) ->
                Height > max_height(left, Pos, HeightMap, HeightMapSize) orelse
                    Height > max_height(right, Pos, HeightMap, HeightMapSize) orelse
                    Height > max_height(up, Pos, HeightMap, HeightMapSize) orelse
                    Height > max_height(down, Pos, HeightMap, HeightMapSize)
            end,
            HeightMap
        ),
    io:format(<<"~b~n">>, [maps:size(VisibleTrees)]).

-spec max_height(
    grid_position:direction(),
    grid_position:t(),
    map_grids:t(integer()),
    {
        non_neg_integer(), non_neg_integer()
    }
) -> integer().
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
