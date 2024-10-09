-module(day_16).

-include_lib("eunit/include/eunit.hrl").

-export([main/1]).

-type adjacent_map() :: #{_Valve :: binary() => _AdjList :: [binary()]}.
-type distance_map() :: #{
    _Valve :: binary() => #{_Valve2 :: binary() => _Dist :: non_neg_integer()}
}.
-type flow_map() :: #{_Valve :: binary() := _Flow :: non_neg_integer()}.

-spec main(1..2) -> ok.
main(1) ->
    {FlowRates, Adjacent} =
        lists:foldl(
            fun(Line, {FlowRates, Adjacent}) ->
                {Valve, Flow, AdjList} = parse_valve(Line),
                {FlowRates#{Valve => Flow}, Adjacent#{Valve => AdjList}}
            end,
            {#{}, #{}},
            io_ext:read_lines(standard_io)
        ),
    Distances = filter_distances(FlowRates, distances(Adjacent), #{<<"AA">> => []}),
    NonZeroValves = maps:keys(maps:filter(fun(_, Flow) -> Flow > 0 end, FlowRates)),
    true = register(
        result_printer,
        spawn(fun() ->
            (fun Loop(MaxTotalFlow) ->
                receive
                    {Visited, NotVisited, TimeLeft, TotalFlow} when TotalFlow > MaxTotalFlow ->
                        io:format(standard_error, <<"~s	~b	~b~n">>, [
                            [lists:reverse(Visited), $_, NotVisited], TimeLeft, TotalFlow
                        ]),
                        Loop(TotalFlow);
                    {_, _, _, _} ->
                        Loop(MaxTotalFlow);
                    {stop, From} ->
                        From ! ok
                end
            end)(
                0
            )
        end)
    ),
    MaxFlow = maximum_flow(FlowRates, Distances, [<<"AA">>], NonZeroValves, 30, 0),
    result_printer ! {stop, self()},
    receive
        ok -> ok
    end,
    io:format(<<"~b~n">>, [MaxFlow]).

-spec eval(flow_map(), distance_map(), [binary()], non_neg_integer()) -> non_neg_integer().
eval(FlowRates, Distances, NonZeroValves, TimeLeft) ->
    eval(FlowRates, Distances, <<"AA">>, NonZeroValves, TimeLeft, 0).

-spec eval(flow_map(), distance_map(), binary(), [binary()], non_neg_integer(), non_neg_integer()) ->
    non_neg_integer().
eval(_, _, _, NonZeroValves, TimeLeft, TotalFlow) when NonZeroValves == []; TimeLeft == 0 ->
    TotalFlow;
eval(FlowRates, Distances, Valve, [NextValve | NonZeroValves], TimeLeft, TotalFlow) ->
    Distance = maps:get(NextValve, maps:get(Valve, Distances)),
    Flow = maps:get(NextValve, FlowRates),
    TimeLeft2 = max(0, TimeLeft - Distance - 1),
    TotalFlow2 = TotalFlow + TimeLeft2 * Flow,
    eval(FlowRates, Distances, NextValve, NonZeroValves, TimeLeft2, TotalFlow2).

-spec for_each_perm(fun(), list()) -> ok.
for_each_perm(Fun, List) -> for_each_perm(Fun, [], List).

-spec for_each_perm(fun(), list(), list()) -> ok.
for_each_perm(Fun, Perm, []) ->
    Fun(Perm);
for_each_perm(Fun, Perm, List) ->
    lists:foreach(
        fun(Elt) ->
            for_each_perm(Fun, [Elt | Perm], lists:delete(Elt, List))
        end,
        List
    ).

-spec maximum_flow(
    flow_map(), distance_map(), [binary()], [binary()], non_neg_integer(), non_neg_integer()
) ->
    non_neg_integer().
maximum_flow(_, _, Visited, NotVisited, TimeLeft, TotalFlow) when NotVisited == []; TimeLeft == 0 ->
    result_printer ! {Visited, NotVisited, TimeLeft, TotalFlow},
    TotalFlow;
maximum_flow(FlowRates, Distances, Visited = [PrevValve | _], NotVisited, TimeLeft, TotalFlow) ->
    lists:foldl(
        fun erlang:max/2,
        0,
        lists:map(
            fun(NextValve) ->
                Distance = maps:get(NextValve, maps:get(PrevValve, Distances)),
                Flow = maps:get(NextValve, FlowRates),
                NotVisited2 = lists:delete(NextValve, NotVisited),
                TimeLeft2 = max(0, TimeLeft - Distance - 1),
                TotalFlow2 = TotalFlow + TimeLeft2 * Flow,
                maximum_flow(
                    FlowRates,
                    Distances,
                    [NextValve | Visited],
                    NotVisited2,
                    TimeLeft2,
                    TotalFlow2
                )
            end,
            NotVisited
        )
    ).

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

-spec valve_distances(binary(), adjacent_map()) -> #{binary() => non_neg_integer()}.
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
