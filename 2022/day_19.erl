%% Not Enough Minerals
-module(day_19).

-export([main/1]).

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
        fun(Blueprint = #blueprint{id = ID}) ->
            MaxRobots = max_robots(Blueprint),
            io:format(standard_error, "Blueprint ~p, MaxRobots ~p, TimeLeft ~p~n", [
                Blueprint, MaxRobots, TimeLeft
            ]),
            spawn_link(fun() ->
                Table = ets:new(cache, [private]),
                MaxGeodes = max_geodes(
                    Blueprint, Table, MaxRobots, #resources{ore = 1}, #resources{}, TimeLeft
                ),
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

-spec max_robots(#blueprint{}) -> #resources{}.
max_robots(#blueprint{
    ore = Ore,
    clay = Clay,
    obsidian = Obsidian,
    geode = Geode
}) ->
    (max_resources(
        max_resources(
            max_resources(
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
    #blueprint{}, ets:table(), #resources{}, #resources{}, #resources{}, non_neg_integer()
) ->
    non_neg_integer().
max_geodes(_, _, _, _, #resources{geode = Geode}, _TimeLeft = 0) ->
    Geode;
max_geodes(
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
                    max_geodes(
                        Blueprint,
                        Cache,
                        MaxRobots,
                        Robots,
                        add_resources(Inventory, Robots),
                        TimeLeft - 1
                    )
                    | [
                        %% Try to build a robot.
                        try
                            NumRobots = element(I, Robots),
                            true = (NumRobots < element(I, MaxRobots)),
                            Cost = element(I, Blueprint),
                            Inventory2 = subtract_resources(Inventory, Cost),
                            Inventory3 = add_resources(Inventory2, Robots),
                            Robots2 = setelement(I, Robots, NumRobots + 1),
                            max_geodes(
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

-spec add_resources(#resources{}, #resources{}) -> #resources{}.
add_resources(
    #resources{ore = Ore1, clay = Clay1, obsidian = Obsidian1, geode = Geode1},
    #resources{ore = Ore2, clay = Clay2, obsidian = Obsidian2, geode = Geode2}
) ->
    #resources{
        ore = Ore1 + Ore2,
        clay = Clay1 + Clay2,
        obsidian = Obsidian1 + Obsidian2,
        geode = Geode1 + Geode2
    }.

-spec subtract_resources(#resources{}, #resources{}) -> #resources{}.
subtract_resources(
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

-spec max_resources(#resources{}, #resources{}) -> #resources{}.
max_resources(
    #resources{ore = Ore1, clay = Clay1, obsidian = Obsidian1, geode = Geode1},
    #resources{ore = Ore2, clay = Clay2, obsidian = Obsidian2, geode = Geode2}
) ->
    #resources{
        ore = max(Ore1, Ore2),
        clay = max(Clay1, Clay2),
        obsidian = max(Obsidian1, Obsidian2),
        geode = max(Geode1, Geode2)
    }.

-spec parse_blueprint(binary()) -> #blueprint{}.
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
