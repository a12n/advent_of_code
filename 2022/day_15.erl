-module(day_15).

-export([main/1]).

-spec main(1..2) -> ok.
main(1) ->
    SensorBeaconPairs =
        lists:map(
            fun(<<Line/bytes>>) ->
                [
                    <<"Sensor">>,
                    <<"at">>,
                    <<"x">>,
                    SensorX,
                    <<"y">>,
                    SensorY,
                    <<"closest">>,
                    <<"beacon">>,
                    <<"is">>,
                    <<"at">>,
                    <<"x">>,
                    BeaconX,
                    <<"y">>,
                    BeaconY
                ] =
                    binary:split(Line, [<<" ">>, <<",">>, <<":">>, <<"=">>], [global, trim_all]),
                {
                    {binary_to_integer(SensorY), binary_to_integer(SensorX)},
                    {binary_to_integer(BeaconY), binary_to_integer(BeaconX)}
                }
            end,
            io_ext:read_lines(standard_io)
        ),
    IsSampleInput =
        case SensorBeaconPairs of
            [{{18, 2}, {15, -2}}, {{16, 9}, {16, 10}} | _] -> true;
            _ -> false
        end,
    Row =
        case IsSampleInput of
            true -> 10;
            false -> 2000000
        end,
    CoveredSegments =
        segments:union(
            lists:sort(
                lists:filtermap(
                    fun(SensorBeacon) ->
                        Segment = covered_row_positions(Row, SensorBeacon),
                        case segments:is_empty(Segment) of
                            true -> false;
                            false -> {true, Segment}
                        end
                    end,
                    SensorBeaconPairs
                )
            )
        ),
    RowBeaconX = lists:uniq(
        lists:filtermap(
            fun
                ({_, {Y, X}}) when Y == Row -> {true, X};
                (_) -> false
            end,
            SensorBeaconPairs
        )
    ),
    NumPositions = lists:foldl(
        fun(BeaconX, Num) ->
            case
                lists:any(
                    fun(Segment) -> segments:is_element(BeaconX, Segment) end, CoveredSegments
                )
            of
                true -> Num - 1;
                false -> Num
            end
        end,
        lists:sum(lists:map(fun segments:size/1, CoveredSegments)),
        RowBeaconX
    ),
    io:format(<<"~b~n">>, [NumPositions]).

-spec covered_row_positions(integer(), {grids:pos(integer()), grids:pos(integer())}) ->
    segments:t().
covered_row_positions(Row, {Sensor = {Y, X}, Beacon}) ->
    Dist = grids:taxicab_distance(Sensor, Beacon),
    case Dist - abs(Row - Y) of
        N when N > 0 -> segments:from_endpoints(X - N, X + N);
        _ -> segments:empty()
    end.
