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

-spec main(1..2) -> ok.
main(1) ->
    Blueprints = lists:map(fun parse_blueprint/1, io_ext:read_lines(standard_io)),
    io:format(standard_error, <<"Blueprints ~p~n">>, [Blueprints]),
    QualityLevels = lists:map(
        fun(Blueprint = #{id := ID}) ->
            MaxGeodes = max_geodes(Blueprint, #{ore => 1}, #{}, 24),
            io:format(standard_error, <<"ID ~p, MaxGeodes ~p~n">>, [ID, MaxGeodes]),
            ID * MaxGeodes
        end,
        Blueprints
    ),
    io:format(standard_error, <<"QualityLevels ~p~n">>, [QualityLevels]),
    io:format(<<"~b~n">>, [lists:sum(QualityLevels)]).

-spec max_geodes(blueprint(), resource_map(), resource_map(), non_neg_integer()) ->
    non_neg_integer().
max_geodes(_, _, Inventory, 0) ->
    maps:get(geode, Inventory, 0);
max_geodes(Blueprint, Robots, Inventory, TimeLeft) ->
    %% TODO
    0.

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
