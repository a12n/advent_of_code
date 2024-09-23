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
    Distances = distances(Adjacent),
    NonZeroValves = maps:keys(maps:filter(fun(_, Flow) -> Flow > 0 end, FlowRates)),
    MaxFlow = maximum_flow(FlowRates, Distances, NonZeroValves, <<"AA">>, 30),
    io:format(<<"~b~n">>, [MaxFlow]).

-spec maximum_flow(flow_map(), distance_map(), [binary()], binary(), non_neg_integer()) ->
    non_neg_integer().
maximum_flow(_, _, _, _, TimeLeft) when TimeLeft =< 0 ->
    0;
maximum_flow(FlowRates, Distances, NonZeroValves, PrevValve, TimeLeft) ->
    lists:foldl(
        fun erlang:max/2,
        0,
        lists:map(
            fun(Valve) ->
                Distance = maps:get(Valve, maps:get(PrevValve, Distances)),
                Flow = maps:get(Valve, FlowRates),
                LessTimeLeft = TimeLeft - Distance - 1,
                %% io:format(standard_error, <<"Distance from ~s to ~s is ~b, ~s flow is ~b~n">>, [
                %%     PrevValve, Valve, Distance, Valve, Flow
                %% ]),
                LessTimeLeft * Flow +
                    maximum_flow(
                        FlowRates,
                        Distances,
                        lists:delete(Valve, NonZeroValves),
                        Valve,
                        LessTimeLeft
                    )
            end,
            NonZeroValves
        )
    ).

-spec distances(adjacent_map()) -> distance_map().
distances(Adjacent) ->
    maps:map(fun(Valve, _) -> valve_distances(Valve, Adjacent) end, Adjacent).

-spec valve_distances(binary(), adjacent_map()) -> #{binary() => non_neg_integer()}.
valve_distances(SourceValve, Adjacent) ->
    (fun Loop(Queue, Distances) ->
        try
            {{Dist, Valve}, Queue2} = gb_sets:take_smallest(Queue),
            Distances2 = Distances#{Valve => Dist},
            AdjList = [
                {Dist + 1, AdjValve}
             || AdjValve <- maps:get(Valve, Adjacent, []),
                not maps:is_key(AdjValve, Distances2)
            ],
            Queue3 =
                lists:foldl(
                    fun gb_sets:add_element/2,
                    Queue2,
                    AdjList
                ),
            Loop(Queue3, Distances2)
        catch
            error:_ -> Distances
        end
    end)(
        gb_sets:from_list([{0, SourceValve}]), #{}
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
