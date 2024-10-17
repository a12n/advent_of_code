%% Not Enough Minerals
-module(day_19).

-export([main/1]).

-type blueprint() :: #{id := pos_integer(), product() := resources()}.
-type product() :: resource() | geode.
-type resource() :: ore | clay | obsidian.
-type resources() :: #{resource() := non_neg_integer()}.
-type robots() :: #{product() := non_neg_integer()}.

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
            MaxNeeded = max_needed(Blueprint),
            Robots = #{ore => 1, clay => 0, obsidian => 0, geode => 0},
            Inventory = #{ore => 0, clay => 0, obsidian => 0},
            io:format(standard_error, "Blueprint ~p, MaxNeeded ~p, TimeLeft ~p~n", [
                Blueprint, MaxNeeded, TimeLeft
            ]),
            spawn_link(fun() ->
                Table = ets:new(cache, [private]),
                MaxGeodes = max_geodes(Blueprint, Table, MaxNeeded, Robots, Inventory, TimeLeft),
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
    io:format("~b~n", [Answer]).

-spec max_needed(blueprint()) -> resources().
max_needed(#{ore := OreCost, clay := ClayCost, obsidian := ObsidianCost, geode := GeodeCost}) ->
    max_resources(max_resources(max_resources(OreCost, ClayCost), ObsidianCost), GeodeCost).

-spec max_geodes(blueprint(), ets:table(), resources(), robots(), resources(), non_neg_integer()) ->
    non_neg_integer().
max_geodes(_, _, _, _, _, _TimeLeft = 0) ->
    0;
max_geodes(Blueprint, Cache, MaxNeeded, Robots = #{geode := GeodeRobots}, Inventory, TimeLeft) ->
    CacheKey = {Robots, Inventory, TimeLeft},
    case ets:lookup(Cache, CacheKey) of
        [{_, MaxGeodes}] ->
            MaxGeodes;
        [] ->
            MaxGeodes =
                GeodeRobots +
                    lists:max([
                        %% Don't produce any robots.
                        max_geodes(
                            Blueprint,
                            Cache,
                            MaxNeeded,
                            Robots,
                            add_resources(Inventory, Robots),
                            TimeLeft - 1
                        )
                        | [
                            %% Try to build a robot.
                            try
                                NumRobots = maps:get(Key, Robots),
                                true = NumRobots < maps:get(Key, MaxNeeded, infinity),
                                Cost = maps:get(Key, Blueprint),
                                Inventory2 = subtract_resources(Inventory, Cost),
                                Inventory3 = add_resources(Inventory2, Robots),
                                Robots2 = Robots#{Key := NumRobots + 1},
                                max_geodes(
                                    Blueprint, Cache, MaxNeeded, Robots2, Inventory3, TimeLeft - 1
                                )
                            catch
                                error:_ -> 0
                            end
                         || Key <- [ore, clay, obsidian, geode]
                        ]
                    ]),
            true = ets:insert(Cache, {CacheKey, MaxGeodes}),
            MaxGeodes
    end.

-spec add_resources(resources(), resources()) -> resources().
add_resources(Resources1, Resources2) -> map_resources(fun erlang:'+'/2, Resources1, Resources2).

-spec subtract_resources(resources(), resources()) -> resources().
subtract_resources(Resources1, Resources2) ->
    map_resources(fun(N, M) when N >= M -> N - M end, Resources1, Resources2).

-spec max_resources(resources(), resources()) -> resources().
max_resources(Resources1, Resources2) -> map_resources(fun max/2, Resources1, Resources2).

-spec map_resources(fun((non_neg_integer()) -> non_neg_integer()), resources()) -> resources().
map_resources(Fun, #{ore := Ore, clay := Clay, obsidian := Obsidian}) ->
    #{ore => Fun(Ore), clay => Fun(Clay), obsidian => Fun(Obsidian)}.

-spec map_resources(
    fun((non_neg_integer(), non_neg_integer()) -> non_neg_integer()), resources(), resources()
) -> resources().
map_resources(
    Fun,
    #{ore := Ore1, clay := Clay1, obsidian := Obsidian1},
    #{ore := Ore2, clay := Clay2, obsidian := Obsidian2}
) ->
    #{ore => Fun(Ore1, Ore2), clay => Fun(Clay1, Clay2), obsidian => Fun(Obsidian1, Obsidian2)}.

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
        ore => #{
            ore => binary_to_integer(OreCostOre),
            clay => 0,
            obsidian => 0
        },
        clay => #{
            ore => binary_to_integer(ClayCostOre),
            clay => 0,
            obsidian => 0
        },
        obsidian => #{
            ore => binary_to_integer(ObsidianCostOre),
            clay => binary_to_integer(ObsidianCostClay),
            obsidian => 0
        },
        geode => #{
            ore => binary_to_integer(GeodeCostOre),
            clay => 0,
            obsidian => binary_to_integer(GeodeCostObsidian)
        }
    }.
