-module(day_15).

-include_lib("eunit/include/eunit.hrl").

-export([main/1]).

-spec main(1..2) -> ok.
main(Part) ->
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
    case Part of
        1 ->
            Row =
                case IsSampleInput of
                    true -> 10;
                    false -> 2000000
                end,
            CoveredSegments = segments_covered(Row, SensorBeaconPairs),
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
                            fun(Segment) -> segments:is_element(BeaconX, Segment) end,
                            CoveredSegments
                        )
                    of
                        true -> Num - 1;
                        false -> Num
                    end
                end,
                lists:sum(lists:map(fun segments:size/1, CoveredSegments)),
                RowBeaconX
            ),
            io:format(<<"~b~n">>, [NumPositions]);
        2 ->
            MaxCoord =
                case IsSampleInput of
                    true -> 20;
                    false -> 4000000
                end,
            CoveredSegments =
                lists:filtermap(
                    fun(Row) ->
                        ?debugFmt("Row ~p", [Row]),
                        case segments_covered(Row, SensorBeaconPairs) of
                            [] -> false;
                            Segments -> {true, Segments}
                        end
                    end,
                    lists:seq(0, MaxCoord)
                ),
            ?debugFmt("CoveredSegments ~p", [CoveredSegments]),
            ok
    end.

-spec columns_covered_by_pair(integer(), {grids:pos(integer()), grids:pos(integer())}) ->
    segments:t().
columns_covered_by_pair(Row, {Sensor = {Y, X}, Beacon}) ->
    Distance = grids:taxicab_distance(Sensor, Beacon),
    case Distance - abs(Row - Y) of
        N when N > 0 -> segments:from_endpoints(X - N, X + N);
        _ -> segments:empty()
    end.

-spec columns_covered(integer(), [{grids:pos(integer()), grids:pos(integer())}]) ->
    integer_sets:t().
columns_covered(Row, SensorBeaconPairs) ->
    integer_sets:from_segments(
        lists:filtermap(
            fun(SensorBeacon) ->
                Segment = columns_covered_by_pair(Row, SensorBeacon),
                case segments:is_empty(Segment) of
                    false -> {true, Segment};
                    true -> false
                end
            end,
            SensorBeaconPairs
        )
    ).
