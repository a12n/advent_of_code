%% Not Enough Minerals
-module(day_19).

-export([main/1]).

-type resource() :: ore | clay | obsidian | geode.
-type resource_map() :: #{resource() => non_neg_integer()}.

-type ore_robot() :: #{ore := pos_integer()}.
-type clay_robot() :: #{ore := pos_integer()}.
-type obsidian_robot() :: #{ore := pos_integer(), clay := pos_integer()}.
-type geode_robot() :: #{ore := pos_integer(), obsidian := pos_integer()}.
-type blueprint() :: #{
    id := integer(),
    ore := ore_robot(),
    clay := clay_robot(),
    obsidian := obsidian_robot(),
    geode := geode_robot()
}.

-record(resources, {
    ore = 0 :: non_neg_integer(),
    clay = 0 :: non_neg_integer(),
    obsidian = 0 :: non_neg_integer(),
    geode = 0 :: non_neg_integer() | infinity
}).
-record(blueprint, {
    ore :: #resources{},
    clay :: #resources{},
    obsidian :: #resources{},
    geode :: #resources{},
    id :: integer()
}).

-spec main(1..2) -> ok.
main(Part) ->
    AllBlueprints = lists:map(fun parse_blueprint/1, io_ext:read_lines(standard_io)),
    MainPID = self(),
    {Blueprints, TimeLeft} =
        case Part of
            1 -> {AllBlueprints, 24};
            2 -> {lists_ext:take(3, AllBlueprints), 32}
        end,
    lists:foreach(
        fun(Blueprint = #{id := ID}) ->
            spawn_link(fun() ->
                Table = ets:new(cache, [private]),
                MaxRobots = max_resource_consumption(Blueprint),
                io:format(standard_error, "Blueprint ~p, MaxRobots ~p, TimeLeft ~p~n", [
                    Blueprint, MaxRobots, TimeLeft
                ]),
                MaxGeodes = max_geodes(Blueprint, Table, MaxRobots, #{ore => 1}, #{}, TimeLeft),
                io:format(standard_error, <<"ID ~p, MaxGeodes ~p~n">>, [ID, MaxGeodes]),
                MainPID ! {ID, MaxGeodes}
            end)
        end,
        Blueprints
    ),
    Answer =
        case Part of
            1 ->
                lists:foldl(
                    fun(_, Sum) ->
                        receive
                            {ID, MaxGeodes} -> (ID * MaxGeodes) + Sum
                        end
                    end,
                    0,
                    Blueprints
                );
            2 ->
                lists:foldl(
                    fun(_, Product) ->
                        receive
                            {_, MaxGeodes} -> MaxGeodes * Product
                        end
                    end,
                    1,
                    Blueprints
                )
        end,
    io:format(standard_error, "~b~n", [Answer]).

-spec max_resource_consumption(blueprint()) -> resource_map().
max_resource_consumption(Blueprint) ->
    lists:foldl(
        fun(Map1, Map2) ->
            maps:merge_with(fun(_, Value1, Value2) -> max(Value1, Value2) end, Map1, Map2)
        end,
        #{},
        [maps:get(Resource, Blueprint, #{}) || Resource <- [ore, clay, obsidian, geode]]
    ).

-spec max_robots(#blueprint{}) -> #resources{}.
max_robots(#blueprint{
    ore = Ore,
    clay = Clay,
    obsidian = Obsidian,
    geode = Geode
}) ->
    (max_resources2(
        max_resources2(
            max_resources2(
                Ore,
                Clay
            ),
            Obsidian
        ),
        Geode
    ))#resources{
        geode = infinity
    }.

-spec max_geodes(
    blueprint(), ets:table(), resource_map(), resource_map(), resource_map(), non_neg_integer()
) ->
    non_neg_integer().
max_geodes(_, _, _, _, Inventory, _TimeLeft = 0) ->
    maps:get(geode, Inventory, 0);
max_geodes(Blueprint, Cache, MaxRobots, Robots, Inventory, TimeLeft) ->
    CacheKey = {Robots, Inventory, TimeLeft},
    case ets:lookup(Cache, CacheKey) of
        [{_, MaxGeodes}] ->
            MaxGeodes;
        [] ->
            MaxGeodes =
                lists:max([
                    max_geodes(
                        Blueprint,
                        Cache,
                        MaxRobots,
                        Robots,
                        add_resources(Inventory, Robots),
                        TimeLeft - 1
                    )
                    | lists:map(
                        fun(Key) ->
                            RobotCost = maps:get(Key, Blueprint),
                            try
                                Inventory2 = subtract_resources(Inventory, RobotCost),
                                Inventory3 = add_resources(Inventory2, Robots),
                                Robots2 = maps:update_with(Key, fun(N) -> N + 1 end, 1, Robots),
                                max_geodes(
                                    Blueprint, Cache, MaxRobots, Robots2, Inventory3, TimeLeft - 1
                                )
                            catch
                                error:_ -> 0
                            end
                        end,
                        [
                            Resource
                         || Resource <- [geode, obsidian, clay, ore],
                            maps:get(Resource, Robots, 0) < maps:get(Resource, MaxRobots, infinity)
                        ]
                    )
                ]),
            true = ets:insert(Cache, {CacheKey, MaxGeodes}),
            MaxGeodes
    end.

-spec max_geodes2(
    #blueprint{}, ets:table(), #resources{}, #resources{}, #resources{}, non_neg_integer()
) ->
    non_neg_integer().
max_geodes2(_, _, _, _, #resources{geode = Geode}, _TimeLeft = 0) ->
    Geode;
max_geodes2(
    Blueprint,
    Cache,
    MaxRobots,
    Robots,
    Inventory,
    TimeLeft
) ->
    CacheKey = {Robots, Inventory, TimeLeft},
    case ets:lookup(Cache, CacheKey) of
        [{_, MaxGeodes}] ->
            MaxGeodes;
        [] ->
            MaxGeodes =
                lists:max([
                    %% Don't produce any robots.
                    max_geodes2(
                        Blueprint,
                        Cache,
                        MaxRobots,
                        Robots,
                        add_resources2(Inventory, Robots),
                        TimeLeft - 1
                    )
                    | [
                        %% Try to build a robot.
                        try
                            NumRobots = element(I, Robots),
                            true = (NumRobots < element(I, MaxRobots)),
                            Cost = element(I, Blueprint),
                            Inventory2 = subtract_resources2(Inventory, Cost),
                            Inventory3 = add_resources2(Inventory2, Robots),
                            Robots2 = setelement(I, Robots, NumRobots + 1),
                            max_geodes2(
                                Blueprint, Cache, MaxRobots, Robots2, Inventory3, TimeLeft - 1
                            )
                        catch
                            error:_ -> 0
                        end
                     || I <- [
                            #resources.ore, #resources.clay, #resources.obsidian, #resources.geode
                        ]
                    ]
                ]),
            true = ets:insert(Cache, {CacheKey, MaxGeodes}),
            MaxGeodes
    end.

-spec add_resources(resource_map(), resource_map()) -> resource_map().
add_resources(Resources1, Resources2) ->
    maps:merge_with(fun(_, N, M) -> N + M end, Resources1, Resources2).

-spec add_resources2(#resources{}, #resources{}) -> #resources{}.
add_resources2(
    #resources{ore = Ore1, clay = Clay1, obsidian = Obsidian1, geode = Geode1},
    #resources{ore = Ore2, clay = Clay2, obsidian = Obsidian2, geode = Geode2}
) ->
    #resources{
        ore = Ore1 + Ore2,
        clay = Clay1 + Clay2,
        obsidian = Obsidian1 + Obsidian2,
        geode = Geode1 + Geode2
    }.

-spec subtract_resources(resource_map(), resource_map()) -> resource_map().
subtract_resources(Resources1, Resources2) ->
    maps:fold(
        fun(Key, M, Ans) -> maps:update_with(Key, fun(N) when N >= M -> N - M end, Ans) end,
        Resources1,
        Resources2
    ).

-spec subtract_resources2(#resources{}, #resources{}) -> #resources{}.
subtract_resources2(
    #resources{ore = Ore1, clay = Clay1, obsidian = Obsidian1, geode = Geode1},
    #resources{ore = Ore2, clay = Clay2, obsidian = Obsidian2, geode = Geode2}
) when
    Ore1 >= Ore2,
    Clay1 >= Clay2,
    Obsidian1 >= Obsidian2,
    Geode1 >= Geode2
->
    #resources{
        ore = Ore1 - Ore2,
        clay = Clay1 - Clay2,
        obsidian = Obsidian1 - Obsidian2,
        geode = Geode1 - Geode2
    }.

-spec max_resources2(#resources{}, #resources{}) -> #resources{}.
max_resources2(
    #resources{ore = Ore1, clay = Clay1, obsidian = Obsidian1, geode = Geode1},
    #resources{ore = Ore2, clay = Clay2, obsidian = Obsidian2, geode = Geode2}
) ->
    #resources{
        ore = max(Ore1, Ore2),
        clay = max(Clay1, Clay2),
        obsidian = max(Obsidian1, Obsidian2),
        geode = max(Geode1, Geode2)
    }.

-spec parse_blueprint(binary()) -> blueprint().
parse_blueprint(Line) ->
    [
        <<"Blueprint">>,
        ID,
        <<"ore">>,
        OreCostOre,
        <<"ore">>,
        <<"clay">>,
        ClayCostOre,
        <<"ore">>,
        <<"obsidian">>,
        ObsidianCostOre,
        <<"ore">>,
        ObsidianCostClay,
        <<"clay">>,
        <<"geode">>,
        GeodeCostOre,
        <<"ore">>,
        GeodeCostObsidian,
        <<"obsidian">>
    ] = binary:split(
        Line,
        [
            <<" ">>,
            <<".">>,
            <<":">>,
            <<"Each">>,
            <<"and">>,
            <<"costs">>,
            <<"robot">>
        ],
        [global, trim_all]
    ),
    #{
        id => binary_to_integer(ID),
        ore => #{ore => binary_to_integer(OreCostOre)},
        clay => #{ore => binary_to_integer(ClayCostOre)},
        obsidian => #{
            ore => binary_to_integer(ObsidianCostOre),
            clay => binary_to_integer(ObsidianCostClay)
        },
        geode => #{
            ore => binary_to_integer(GeodeCostOre),
            obsidian => binary_to_integer(GeodeCostObsidian)
        }
    }.

-spec parse_blueprint2(binary()) -> #blueprint{}.
parse_blueprint2(Line) ->
    [
        <<"Blueprint">>,
        ID,
        <<"ore">>,
        OreCostOre,
        <<"ore">>,
        <<"clay">>,
        ClayCostOre,
        <<"ore">>,
        <<"obsidian">>,
        ObsidianCostOre,
        <<"ore">>,
        ObsidianCostClay,
        <<"clay">>,
        <<"geode">>,
        GeodeCostOre,
        <<"ore">>,
        GeodeCostObsidian,
        <<"obsidian">>
    ] = binary:split(
        Line,
        [
            <<" ">>,
            <<".">>,
            <<":">>,
            <<"Each">>,
            <<"and">>,
            <<"costs">>,
            <<"robot">>
        ],
        [global, trim_all]
    ),
    #blueprint{
        id = binary_to_integer(ID),
        ore = #resources{ore = binary_to_integer(OreCostOre)},
        clay = #resources{ore = binary_to_integer(ClayCostOre)},
        obsidian = #resources{
            ore = binary_to_integer(ObsidianCostOre),
            clay = binary_to_integer(ObsidianCostClay)
        },
        geode = #resources{
            ore = binary_to_integer(GeodeCostOre),
            obsidian = binary_to_integer(GeodeCostObsidian)
        }
    }.
