-module(day_16).

-include_lib("eunit/include/eunit.hrl").

-export([main/1]).

-type valve_id() :: non_neg_integer().
-type adjacent_map() :: #{_Valve :: valve_id() => _AdjList :: [valve_id()]}.
-type distance_map() :: #{
    _Valve :: valve_id() => #{_Valve2 :: valve_id() => _Dist :: non_neg_integer()}
}.
-type flow_map() :: #{_Valve :: valve_id() := _Flow :: non_neg_integer()}.

-spec main(1..2) -> ok.
main(Part) ->
    _Table = ets:new(cache, [named_table]),
    {FlowRates, Distances} = parse_input(io_ext:read_lines(standard_io)),
    NonZeroValves = lists:sort(maps:keys(FlowRates)),
    MaxFlow =
        case Part of
            1 ->
                maximum_flow(
                    FlowRates, Distances, -1, NonZeroValves, 30
                );
            2 ->
                maximum_flow2(
                    FlowRates, Distances, -1, -1, NonZeroValves, 26, 26
                )
        end,
    io:format(<<"~b~n">>, [MaxFlow]).

-spec maximum_flow(
    flow_map(), distance_map(), valve_id(), [valve_id()], non_neg_integer()
) ->
    non_neg_integer().
maximum_flow(_, _, _, NotOpened, TimeLeft) when NotOpened == []; TimeLeft == 0 ->
    0;
maximum_flow(FlowRates, Distances, PrevValve, NotOpened, TimeLeft) ->
    CacheKey = {PrevValve, NotOpened, TimeLeft},
    case ets:lookup(cache, CacheKey) of
        [{_, TotalFlow}] ->
            TotalFlow;
        [] ->
            TotalFlow =
                lists:foldl(
                    fun erlang:max/2,
                    0,
                    lists:map(
                        fun(NextValve) ->
                            Distance = maps:get(NextValve, maps:get(PrevValve, Distances)),
                            Flow = maps:get(NextValve, FlowRates),
                            NotOpened2 = lists:delete(NextValve, NotOpened),
                            TimeLeft2 = max(0, TimeLeft - Distance - 1),
                            TimeLeft2 * Flow +
                                maximum_flow(
                                    FlowRates,
                                    Distances,
                                    NextValve,
                                    NotOpened2,
                                    TimeLeft2
                                )
                        end,
                        NotOpened
                    )
                ),
            true = ets:insert(cache, {CacheKey, TotalFlow}),
            TotalFlow
    end.

-spec maximum_flow2(
    flow_map(),
    distance_map(),
    valve_id(),
    valve_id(),
    [valve_id()],
    non_neg_integer(),
    non_neg_integer()
) ->
    non_neg_integer().
maximum_flow2(
    FlowRates, Distances, ValveA, _ValveB, NotOpened, TimeLeftA, _TimeLeftB = 0
) ->
    maximum_flow(FlowRates, Distances, ValveA, NotOpened, TimeLeftA);
maximum_flow2(
    FlowRates, Distances, _ValveA, ValveB, NotOpened, _TimeLeftA = 0, TimeLeftB
) ->
    maximum_flow(FlowRates, Distances, ValveB, NotOpened, TimeLeftB);
maximum_flow2(
    FlowRates,
    Distances,
    PrevValveA,
    PrevValveB,
    NotOpened,
    TimeLeftA,
    TimeLeftB
) ->
    CacheKey = {{PrevValveA, PrevValveB}, NotOpened, {TimeLeftA, TimeLeftB}},
    case ets:lookup(cache, CacheKey) of
        [{_, TotalFlow}] ->
            TotalFlow;
        [] ->
            TotalFlow =
                lists:foldl(
                    fun erlang:max/2,
                    0,
                    lists:map(
                        fun(NextValveA) ->
                            DistanceA = maps:get(NextValveA, maps:get(PrevValveA, Distances)),
                            FlowA = maps:get(NextValveA, FlowRates),
                            NotOpened2 = lists:delete(NextValveA, NotOpened),
                            TimeLeftA2 = max(0, TimeLeftA - DistanceA - 1),
                            TimeLeftA2 * FlowA +
                                lists:foldl(
                                    fun erlang:max/2,
                                    0,
                                    lists:map(
                                        fun(NextValveB) ->
                                            DistanceB = maps:get(
                                                NextValveB, maps:get(PrevValveB, Distances)
                                            ),
                                            FlowB = maps:get(NextValveB, FlowRates),
                                            NotOpened3 = lists:delete(NextValveB, NotOpened2),
                                            TimeLeftB2 = max(0, TimeLeftB - DistanceB - 1),
                                            TimeLeftB2 * FlowB +
                                                maximum_flow2(
                                                    FlowRates,
                                                    Distances,
                                                    NextValveA,
                                                    NextValveB,
                                                    NotOpened3,
                                                    TimeLeftA2,
                                                    TimeLeftB2
                                                )
                                        end,
                                        NotOpened2
                                    )
                                )
                        end,
                        NotOpened
                    )
                ),
            true = ets:insert(cache, {CacheKey, TotalFlow}),
            TotalFlow
    end.

-spec distances(adjacent_map()) -> distance_map().
distances(Adjacent) ->
    maps:map(fun(Valve, _) -> valve_distances(Valve, Adjacent) end, Adjacent).

-spec valve_distances(binary(), adjacent_map()) -> #{valve_id() => non_neg_integer()}.
valve_distances(SourceValve, Adjacent) ->
    (fun Loop(Queue, Visited, Distances) ->
        try
            {{Dist, Valve}, Queue2} = gb_sets:take_smallest(Queue),
            AdjList = [
                {Dist + 1, AdjValve}
             || AdjValve <- maps:get(Valve, Adjacent, []),
                not gb_sets:is_element(AdjValve, Visited)
            ],
            Queue3 =
                lists:foldl(
                    fun gb_sets:add_element/2,
                    Queue2,
                    AdjList
                ),
            Visited2 =
                lists:foldl(
                    fun gb_sets:add_element/2,
                    Visited,
                    [AdjValve || {_, AdjValve} <- AdjList]
                ),
            Distances2 = Distances#{Valve => Dist},
            Loop(Queue3, Visited2, Distances2)
        catch
            error:_ -> Distances
        end
    end)(
        gb_sets:from_list([{0, SourceValve}]),
        gb_sets:from_list([SourceValve]),
        #{}
    ).

-spec parse_valve(binary()) -> {binary(), non_neg_integer(), [binary()]}.
parse_valve(Line) ->
    [
        <<"Valve">>,
        Valve,
        <<"has">>,
        <<"flow">>,
        <<"rate">>,
        FlowStr,
        <<"tunnel", _/bytes>>,
        <<"lead", _/bytes>>,
        <<"to">>,
        <<"valve", _/bytes>>
        | AdjList
    ] =
        binary:split(Line, [<<" ">>, <<",">>, <<"=">>, <<";">>], [global, trim_all]),
    {Valve, binary_to_integer(FlowStr), AdjList}.

-spec parse_input([binary()]) -> {flow_map(), adjacent_map()}.
parse_input(Lines) ->
    {FlowRates, Adjacent} =
        lists:foldl(
            fun(Line, {FlowRates, Adjacent}) ->
                {Valve, Flow, AdjList} = parse_valve(Line),
                {FlowRates#{Valve => Flow}, Adjacent#{Valve => AdjList}}
            end,
            {#{}, #{}},
            Lines
        ),
    NonZeroFlowRates = maps:filter(fun(_, Flow) -> Flow > 0 end, FlowRates),
    %% Enumerate valves, replace string keys with integer indices.
    ValveIndices = maps:from_list([
        {Valve, Index}
     || {Index, Valve} <- lists:enumerate(-1, lists:sort([<<"AA">> | maps:keys(NonZeroFlowRates)]))
    ]),
    %% Rename valves in flow rates.
    NonZeroFlowRates2 = maps:fold(
        fun(Valve, Flow, FlowRatesAns) ->
            Index = maps:get(Valve, ValveIndices),
            FlowRatesAns#{Index => Flow}
        end,
        #{},
        NonZeroFlowRates
    ),
    %% Rename valves in distances, remove zero valves from the distances map.
    Distances2 = maps:fold(
        fun
            (Valve, ValveDistances, DistancesAns) when
                Valve == <<"AA">>; is_map_key(Valve, NonZeroFlowRates)
            ->
                Index = maps:get(Valve, ValveIndices),
                DistancesAns#{
                    Index => maps:fold(
                        fun
                            (NextValve, Distance, ValveDistancesAns) when
                                is_map_key(NextValve, NonZeroFlowRates)
                            ->
                                NextIndex = maps:get(NextValve, ValveIndices),
                                ValveDistancesAns#{NextIndex => Distance};
                            (_, _, ValveDistancesAns) ->
                                %% Ignore zero flow valve.
                                ValveDistancesAns
                        end,
                        #{},
                        ValveDistances
                    )
                };
            (_, _, DistancesAns) ->
                %% Ignore zero flow valve.
                DistancesAns
        end,
        #{},
        distances(Adjacent)
    ),
    %% Return structures with renamed valves.
    {NonZeroFlowRates2, Distances2}.
