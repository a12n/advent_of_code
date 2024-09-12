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
            ExcludedColumns = row_columns_covered(Row, SensorBeaconPairs),
            BeaconColumns = integer_sets:from_list(
                lists:filtermap(
                    fun
                        ({_, {Y, X}}) when Y == Row -> {true, X};
                        (_) -> false
                    end,
                    SensorBeaconPairs
                )
            ),
            io:format(<<"~b~n">>, [
                integer_sets:size(integer_sets:subtract(ExcludedColumns, BeaconColumns))
            ]);
        2 ->
            MaxCoord =
                case IsSampleInput of
                    true -> 20;
                    false -> 4000000
                end,
            AllRows = integer_sets:from_segment(segments:from_endpoints(0, MaxCoord)),
            AllColunms = integer_sets:from_segment(segments:from_endpoints(0, MaxCoord)),
            %% For all rows, exclude columns, keep only rows where
            %% possible set of columns isn't empty.
            [{Row, Colunms}] = rows_columns_covered(
                AllRows, AllColunms, SensorBeaconPairs
            ),
            %% For the found row, run the same function for everything
            %% transposed.
            [{Col, _}] = rows_columns_covered(
                Colunms,
                integer_sets:from_list([Row]),
                lists:map(
                    fun({{SensorY, SensorX}, {BeaconY, BeaconX}}) ->
                        {{SensorX, SensorY}, {BeaconX, BeaconY}}
                    end,
                    SensorBeaconPairs
                )
            ),
            io:format(<<"~b~n">>, [Row * Col])
    end.

-spec row_columns_covered_by_pair(integer(), {grids:pos(integer()), grids:pos(integer())}) ->
    segments:t().
row_columns_covered_by_pair(Row, {Sensor = {Y, X}, Beacon}) ->
    Distance = grids:taxicab_distance(Sensor, Beacon),
    case Distance - abs(Row - Y) of
        N when N > 0 -> segments:from_endpoints(X - N, X + N);
        _ -> segments:empty()
    end.

-spec row_columns_covered(integer(), [{grids:pos(integer()), grids:pos(integer())}]) ->
    integer_sets:t().
row_columns_covered(Row, SensorBeaconPairs) ->
    integer_sets:from_segments(
        lists:filtermap(
            fun(SensorBeacon) ->
                Segment = row_columns_covered_by_pair(Row, SensorBeacon),
                case segments:is_empty(Segment) of
                    false -> {true, Segment};
                    true -> false
                end
            end,
            SensorBeaconPairs
        )
    ).

-spec rows_columns_covered(integer_sets:t(), integer_sets:t(), [
    {grids:pos(integer()), grids:pos(integer())}
]) -> [{integer(), integer_sets:t()}].
rows_columns_covered(AllRows, AllColunms, SensorBeaconPairs) ->
    integer_sets:fold(
        fun(Row, List) ->
            Colunms = row_columns_covered(Row, SensorBeaconPairs),
            case integer_sets:subtract(AllColunms, Colunms) of
                [] -> List;
                Cols -> [{Row, Cols} | List]
            end
        end,
        [],
        AllRows
    ).
