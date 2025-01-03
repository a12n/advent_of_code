%% Blizzard Basin
-module(day_24).

-export([main/1]).

-spec main(1..2) -> ok.
main(Part) ->
    {Blizzards, Extent, Begin, End} = parse_input(io_ext:read_lines(standard_io)),
    io:format(standard_error, "Extent ~p, Begin ~p, End ~p, Blizzards~n~ts", [
        Extent,
        Begin,
        End,
        grids:to_iodata(
            maps:map(fun(_, Dir) -> grids:dir_to_char(Dir) end, Blizzards),
            Extent,
            #{
                blank => $.
            }
        )
    ]),
    case Part of
        1 ->
            {Distance, _} = min_distance(Blizzards, Extent, Begin, End, 0),
            io:format("~b~n", [Distance]);
        2 ->
            {_, Time1} = min_distance(Blizzards, Extent, Begin, End, 0),
            {_, Time2} = min_distance(Blizzards, Extent, End, Begin, Time1),
            {_, Time3} = min_distance(Blizzards, Extent, Begin, End, Time2),
            io:format("~b~n", [Time3])
    end.

-spec min_distance(
    grids:grid(integer()),
    grids:extent(integer()),
    grids:pos(integer()),
    grids:pos(integer()),
    non_neg_integer()
) -> {non_neg_integer(), non_neg_integer()}.
min_distance(Blizzards, Extent, Begin, End, Time0) ->
    (fun Loop(Queue) ->
        case gb_sets:take_smallest(Queue) of
            {{Distance, Time, End}, _} ->
                %% Reached the end position, return the minimum
                %% distance and the time.
                {Distance, Time};
            {{Distance, Time, Pos}, Queue2} ->
                %% Enumerate all possible positions for the next move.
                AdjacentList =
                    [
                        grids:add_pos(Pos, grids:dir_to_pos(Dir))
                     || Dir <- [up, down, left, right]
                    ],
                %% Filter invalid positions due to grid extent.
                AdjacentList2 = [
                    NextPos
                 || NextPos <- AdjacentList,
                    grids:is_valid_pos(NextPos, Extent) orelse NextPos == End
                ],
                %% Filter invalid positions due to intersections with
                %% a blizzard. Also may try to wait in current
                %% position.
                AdjacentList3 = [
                    NextPos
                 || NextPos <- [Pos | AdjacentList2],
                    not intersects(Blizzards, Extent, NextPos, Time + 1)
                ],
                %% Enqueue next states and continue.
                Loop(
                    lists:foldl(
                        fun gb_sets:add_element/2,
                        Queue2,
                        [
                            {Distance + 1, Time + 1, NextPos}
                         || NextPos <- AdjacentList3
                        ]
                    )
                )
        end
    end)(
        gb_sets:from_list([{0, Time0, Begin}])
    ).

-spec intersects(
    grids:grid(integer()), grids:extent(integer()), grids:pos(integer()), non_neg_integer()
) -> boolean().
intersects(Blizzards, _, Pos, 0) ->
    maps:is_key(Pos, Blizzards);
intersects(Blizzards, {{MinRow, MinCol}, {MaxRow, MaxCol}}, {Row, Col}, Time) ->
    lists:any(
        fun({Dir, Pos}) ->
            %% Project positions back in time. E.g., if there is a
            %% right moving blizzard at the current expedition
            %% position, then it was at {CurrentRow, CurrentColumn -
            %% Time} some time ago (modulo the grid size).
            maps:get(Pos, Blizzards, undefined) == Dir
        end,
        [
            {up, {indices:remap(Row + Time, MinRow, MaxRow), Col}},
            {down, {indices:remap(Row - Time, MinRow, MaxRow), Col}},
            {left, {Row, indices:remap(Col + Time, MinCol, MaxCol)}},
            {right, {Row, indices:remap(Col - Time, MinCol, MaxCol)}}
        ]
    ).

-spec parse_input([binary()]) ->
    {grids:grid(integer()), grids:extent(integer()), grids:pos(integer()), grids:pos(integer())}.
parse_input(Lines) ->
    Grid = grids:from_lines(Lines),
    Extent = {{MinRow, MinCol}, {MaxRow, MaxCol}} = grids:extent(Grid),
    io:format(standard_error, "Extent = ~p, Grid =~n~s", [
        Extent, grids:to_iodata(Grid, Extent, #{})
    ]),
    %% Find begin and end positions.
    {BeginCol, EndCol} =
        lists:foldl(
            fun(Col, {BeginCol, EndCol}) ->
                {
                    case map_get({MinRow, Col}, Grid) of
                        $. -> Col;
                        $# -> BeginCol
                    end,
                    case map_get({MaxRow, Col}, Grid) of
                        $. -> Col;
                        $# -> EndCol
                    end
                }
            end,
            {undefined, undefined},
            lists:seq(MinCol, MaxCol)
        ),
    Begin = {MinRow, BeginCol},
    End = {MaxRow, EndCol},
    %% Reduce extent as the borders are removed.
    Extent2 = {{MinRow + 1, MinCol + 1}, {MaxRow - 1, MaxCol - 1}},
    %% Filter grid, keep only blizzards. Convert to duplicate_bag ETS table.
    Blizzards = maps:filtermap(
        fun
            (_, $#) -> false;
            (_, $.) -> false;
            (_, $^) -> {true, up};
            (_, $<) -> {true, left};
            (_, $>) -> {true, right};
            (_, $v) -> {true, down}
        end,
        Grid
    ),
    %% Return result.
    {Blizzards, Extent2, Begin, End}.
