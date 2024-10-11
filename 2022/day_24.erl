%% Blizzard Basin
-module(day_24).

-export([main/1]).

-spec main(1..2) -> ok.
main(1) ->
    {Grid, Extent, Begin, End} = parse_input(io_ext:read_lines(standard_io)),
    ok.

-spec parse_input([binary()]) ->
    {
        grids:grid(integer()),
        {grids:pos(integer()), grids:pos(integer())},
        grids:pos(integer()),
        grids:pos(integer())
    }.
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
    io:format(standard_error, "Begin ~p, End ~p~n", [Begin, End]),
    %% Filter grid, keep only blizzards. Also reduce extent as the
    %% borders are removed.
    Extent2 = {{MinRow + 1, MinCol + 1}, {MaxRow - 1, MaxCol - 1}},
    Grid2 = maps:filtermap(
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
    io:format(standard_error, "Extent2 = ~p, Grid2 =~n~s", [
        Extent2,
        grids:to_iodata(
            maps:map(
                fun
                    (_, up) -> "^";
                    (_, left) -> "<";
                    (_, right) -> ">";
                    (_, down) -> "v"
                end,
                Grid2
            ),
            Extent2,
            #{blank => $.}
        )
    ]),
    %% Return result.
    {Grid2, Extent2, Begin, End}.
