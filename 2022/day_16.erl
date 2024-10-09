-module(day_16).

-include_lib("eunit/include/eunit.hrl").

-export([main/1]).

-type valve_id() :: binary().
-type adjacent_map() :: #{_Valve :: valve_id() => _AdjList :: [valve_id()]}.
-type distance_map() :: #{
    _Valve :: valve_id() => #{_Valve2 :: valve_id() => _Dist :: non_neg_integer()}
}.
-type flow_map() :: #{_Valve :: valve_id() := _Flow :: non_neg_integer()}.

-spec main(1..2) -> ok.
main(Part) ->
    _Table = ets:new(cache, [named_table]),
    {FlowRates, Distances} = parse_input(io_ext:read_lines(standard_io)),
    NonZeroValves = lists:sort(maps:keys(maps:filter(fun(_, Flow) -> Flow > 0 end, FlowRates))),
    true = register(
        result_printer,
        spawn(fun() ->
            (fun Loop(MaxTotalFlow) ->
                receive
                    {Opened, NotOpened, TimeLeft, TotalFlow} when TotalFlow > MaxTotalFlow ->
                        io:format(standard_error, <<"~s	~b	~b~n">>, [
                            [lists:reverse(Opened), $_, NotOpened], TimeLeft, TotalFlow
                        ]),
                        Loop(TotalFlow);
                    {_, _, _, _} ->
                        Loop(MaxTotalFlow);
                    dump_cache ->
                        Info = ets:info(cache),
                        io:format(standard_error, <<"cache: size ~b, memory ~b~n">>, [
                            proplists:get_value(size, Info),
                            proplists:get_value(memory, Info)
                        ]),
                        Loop(MaxTotalFlow);
                    {stop, From} ->
                        From ! ok
                end
            end)(
                0
            )
        end)
    ),
    {ok, TimerRef} = timer:send_interval(1000, result_printer, dump_cache),
    MaxFlow =
        case Part of
            1 ->
                maximum_flow(
                    FlowRates, Distances, [<<"AA">>], NonZeroValves, 30
                );
            2 ->
                maximum_flow2(
                    FlowRates, Distances, [<<"AA">>], [<<"AA">>], NonZeroValves, 26, 26
                )
        end,
    {ok, _} = timer:cancel(TimerRef),
    result_printer ! {stop, self()},
    receive
        ok -> ok
    end,
    io:format(<<"~b~n">>, [MaxFlow]).

-spec maximum_flow(
    flow_map(), distance_map(), [valve_id()], [valve_id()], non_neg_integer()
) ->
    non_neg_integer().
maximum_flow(_, _, _Opened, NotOpened, TimeLeft) when NotOpened == []; TimeLeft == 0 ->
    0;
maximum_flow(FlowRates, Distances, Opened = [PrevValve | _], NotOpened, TimeLeft) ->
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
                                    [NextValve | Opened],
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
    [valve_id()],
    [valve_id()],
    [valve_id()],
    non_neg_integer(),
    non_neg_integer()
) ->
    non_neg_integer().
maximum_flow2(
    FlowRates, Distances, OpenedA, _OpenedB, NotOpened, TimeLeftA, _TimeLeftB = 0
) ->
    maximum_flow(FlowRates, Distances, OpenedA, NotOpened, TimeLeftA);
maximum_flow2(
    FlowRates, Distances, _OpenedA, OpenedB, NotOpened, _TimeLeftA = 0, TimeLeftB
) ->
    maximum_flow(FlowRates, Distances, OpenedB, NotOpened, TimeLeftB);
maximum_flow2(
    FlowRates,
    Distances,
    OpenedA = [PrevValveA | _],
    OpenedB = [PrevValveB | _],
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
                            OpenedA2 = [NextValveA | OpenedA],
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
                                            OpenedB2 = [NextValveB | OpenedB],
                                            NotOpened3 = lists:delete(NextValveB, NotOpened2),
                                            TimeLeftB2 = max(0, TimeLeftB - DistanceB - 1),
                                            TimeLeftB2 * FlowB +
                                                maximum_flow2(
                                                    FlowRates,
                                                    Distances,
                                                    OpenedA2,
                                                    OpenedB2,
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

-spec filter_distances(flow_map(), distance_map(), map()) -> distance_map().
filter_distances(FlowRates, Distances, Keep) ->
    maps:filtermap(
        fun
            (Valve, AdjDistances) when
                is_map_key(Valve, Keep);
                map_get(Valve, FlowRates) > 0
            ->
                {true,
                    maps:filter(
                        fun
                            (AdjValve, _) when AdjValve == Valve -> false;
                            (AdjValve, _) when map_get(AdjValve, FlowRates) > 0 -> true;
                            (_, _) -> false
                        end,
                        AdjDistances
                    )};
            (_, _) ->
                false
        end,
        Distances
    ).

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
    Distances = filter_distances(FlowRates, distances(Adjacent), #{<<"AA">> => []}),
    {FlowRates, Distances}.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

distances_test() ->
    ?assertEqual(
        #{
            a => #{
                b => 1,
                c => 2,
                d => 1,
                e => 2,
                h => 5,
                j => 2
            },
            b => #{
                c => 1,
                d => 2,
                e => 3,
                h => 6,
                j => 3
            },
            c => #{
                b => 1,
                d => 1,
                e => 2,
                h => 5,
                j => 4
            },
            d => #{
                b => 2,
                c => 1,
                e => 1,
                h => 4,
                j => 3
            },
            e => #{
                b => 3,
                c => 2,
                d => 1,
                h => 3,
                j => 4
            },
            h => #{
                b => 6,
                c => 5,
                d => 4,
                e => 3,
                j => 7
            },
            j => #{
                b => 3,
                c => 4,
                d => 3,
                e => 4,
                h => 7
            }
        },
        filter_distances(
            _FlowRates = #{
                a => 0,
                b => 13,
                c => 2,
                d => 20,
                e => 3,
                f => 0,
                g => 0,
                h => 22,
                i => 0,
                j => 21
            },
            distances(
                _Adjacent = #{
                    a => [d, i, b],
                    b => [c, a],
                    c => [d, b],
                    d => [a, e, c],
                    e => [d, f],
                    f => [e, g],
                    g => [f, h],
                    h => [g],
                    i => [a, j],
                    j => [i]
                }
            ),
            _Keep = #{a => []}
        )
    ).

-endif.
