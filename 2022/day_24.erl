%% Blizzard Basin
-module(day_24).

-export([main/1]).

%% -define(UP, 2#0100).
%% -define(DOWN, 2#0010).
%% -define(LEFT, 2#1000).
%% -define(RIGHT, 2#0001).

%% How to check intersections of the expedition with blizzards:
%% - We're at minute T.
%% - Move current expedition position up, left, right, and down -T positions.
%% - Does this intersects with blizzards (they're really static in their map).

-spec main(1..2) -> ok.
main(1) ->
    {Blizzards, Extent, Begin, End} = parse_input(io_ext:read_lines(standard_io)),
    io:format(standard_error, "Extent ~p, Begin ~p, End ~p, Blizzards~n~ts", [
        Extent,
        Begin,
        End,
        grids:to_iodata(
            maps:map(
                fun
                    (_, up) -> $↑;
                    (_, left) -> $←;
                    (_, right) -> $→;
                    (_, down) -> $↓
                end,
                Blizzards
            ),
            Extent,
            #{blank => $.}
        )
    ]),
    ok.

-spec parse_input([binary()]) ->
    {grids:grid(integer()), girds:extent(integer()), grids:pos(integer()), grids:pos(integer())}.
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
