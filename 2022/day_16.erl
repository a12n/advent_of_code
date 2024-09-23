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
    {FlowMap, AdjacentMap} =
        lists:foldl(
            fun(Line, {FlowMap, AdjacentMap}) ->
                {Valve, Flow, AdjList} = parse_valve(Line),
                {FlowMap#{Valve => Flow}, AdjacentMap#{Valve => AdjList}}
            end,
            {#{}, #{}},
            io_ext:read_lines(standard_io)
        ),
    DistanceMap = distances(AdjacentMap),
    ?debugFmt("FlowMap ~p, AdjacentMap ~p, DistanceMap ~p", [FlowMap, AdjacentMap, DistanceMap]),
    %% Distance = maps:from_list([{ID, distances(ID, Adjacent)} || ID <- maps:keys(Adjacent)]),
    %% ?debugFmt("Distance ~p", [Distance]),
    %% io:format(standard_error, <<"~p valves ~p~n">>, [maps:size(Valves), Valves]),
    %% NonZeroValves = maps:keys(maps:filter(fun(_, {Flow, _}) -> Flow > 0 end, Valves)),
    %% io:format(standard_error, <<"NonZeroValves ~p~n">>, [NonZeroValves]),
    %% io:format(graphviz:to_iodata(Valves)),
    %% Valves = maps:from_list([
    %%     {ID, {Flow, AdjList}}
    %%  || {ID, Flow, AdjList} <- lists:map(fun parse_valve/1, io_ext:read_lines(standard_io))
    %% ]),
    %% ?debugFmt("Valves ~p", [Valves]),
    %% io:format("~b~n", [maximum_flow(Valves, 30, <<"AA">>)]),
    ok.

-spec distances(adjacent_map()) -> distance_map().
distances(AdjacentMap) ->
    maps:map(fun(Valve, _) -> valve_distances(Valve, AdjacentMap) end, AdjacentMap).

-spec valve_distances(binary(), adjacent_map()) -> #{binary() => non_neg_integer()}.
valve_distances(SourceValve, AdjacentMap) ->
    (fun Loop(Queue, Distances) ->
        try
            {{Dist, Valve}, Queue2} = gb_sets:take_smallest(Queue),
            Distances2 = Distances#{Valve => Dist},
            AdjList = [
                {Dist + 1, AdjValve}
             || AdjValve <- maps:get(Valve, AdjacentMap, []),
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

maximum_flow(Valves, TotalTime, Start) ->
    (fun
        Loop(_, TimeLeft, ID) when TimeLeft =< 0 ->
            %% ?debugFmt("TimeLeft ~p, ID ~p", [TimeLeft, ID]),
            %% io:format(standard_error, <<"No time left @ ~s~n">>, [, ID]),
            0;
        Loop(Closed, _, _) when map_size(Closed) == 0 ->
            %% ?debugFmt("Closed ~p", [Closed]),
            io:format(standard_error, <<"All opened~n">>, []),
            0;
        Loop(Closed, TimeLeft, ID) ->
            {Flow, Adjacent} = maps:get(ID, Valves),
            %% ?debugFmt("Closed ~p, TimeLeft ~p, ID ~p, Flow ~p, Adjacent ~p", [
            %%     Closed, TimeLeft, ID, Flow, Adjacent
            %% ]),
            case maps:find(ID, Closed) of
                {ok, _} when Flow > 0 ->
                    WhenOpened =
                        Flow * (TimeLeft - 1) +
                            lists:max([
                                Loop(maps:remove(ID, Closed), TimeLeft - 2, AdjID)
                             || AdjID <- Adjacent
                            ]),
                    %% ?debugFmt("WhenOpened ~p", [WhenOpened]),
                    WhenNotOpened =
                        lists:max([Loop(Closed, TimeLeft - 1, AdjID) || AdjID <- Adjacent]),
                    %% ?debugFmt("WhenNotOpened ~p", [WhenNotOpened]),
                    max(WhenOpened, WhenNotOpened);
                {ok, _} ->
                    %% Only mark as opened.
                    WhenNotOpened =
                        lists:max([
                            Loop(maps:remove(ID, Closed), TimeLeft - 1, AdjID)
                         || AdjID <- Adjacent
                        ]),
                    %% ?debugFmt("WhenNotOpened ~p, zero flow", [WhenNotOpened]),
                    WhenNotOpened;
                error ->
                    %% Already opened
                    AlreadyOpened =
                        lists:max([Loop(Closed, TimeLeft - 1, AdjID) || AdjID <- Adjacent]),
                    %% ?debugFmt("AlreadyOpened ~p", [AlreadyOpened]),
                    AlreadyOpened
            end
    end)(
        maps:map(fun(_, _) -> [] end, Valves), TotalTime, Start
    ).
