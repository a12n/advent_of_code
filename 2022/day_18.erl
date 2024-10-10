%% Boiling Boulders
-module(day_18).

-export([main/1]).

-define(AIR_INSIDE, $o).
-define(AIR_OUTSIDE, $.).
-define(LAVA, $#).

-spec main(1..2) -> ok.
main(Part) ->
    Cubes = maps:from_list(
        [
            {Pos, ?LAVA}
         || Pos <-
                lists:map(
                    fun(<<Line/bytes>>) ->
                        [X, Y, Z] = binary:split(Line, <<",">>, [global, trim]),
                        {binary_to_integer(X), binary_to_integer(Y), binary_to_integer(Z)}
                    end,
                    io_ext:read_lines(standard_io)
                )
        ]
    ),
    io:format(<<"~b~n">>, [
        surface_area(
            case Part of
                1 -> Cubes;
                2 -> expand_steam(Cubes)
            end
        )
    ]).

-spec surface_area(map()) -> non_neg_integer().
surface_area(Cubes) ->
    length([
        AdjPos
     || {{X, Y, Z}, Type} <- maps:to_list(Cubes),
        Type =/= ?AIR_OUTSIDE,
        AdjPos <- [
            {X - 1, Y, Z},
            {X + 1, Y, Z},
            {X, Y - 1, Z},
            {X, Y + 1, Z},
            {X, Y, Z - 1},
            {X, Y, Z + 1}
        ],
        maps:get(AdjPos, Cubes, ?AIR_OUTSIDE) == ?AIR_OUTSIDE
    ]).

-spec expand_steam(map()) -> map().
expand_steam(Cubes) ->
    {MinPos = {MinX, MinY, MinZ}, {MaxX, MaxY, MaxZ}} = grids3:extent(Cubes),
    %% Introduce explicit outside air voxels into the map.
    ExplicitCubes0 =
        maps:merge_with(
            fun(_, Type, _) -> Type end,
            Cubes,
            maps:from_list([
                {{X, Y, Z}, ?AIR_OUTSIDE}
             || X <- lists:seq(MinX, MaxX),
                Y <- lists:seq(MinY, MaxY),
                Z <- lists:seq(MinZ, MaxZ)
            ])
        ),
    %% Start expansion at MinPos, it must be an outside air voxel.
    ?AIR_OUTSIDE = maps:get(MinPos, ExplicitCubes0),
    %% Do the expansion.
    Cubes2 =
        (fun Loop(Pos = {X, Y, Z}, ExplicitCubes) ->
            case maps:find(Pos, ExplicitCubes) of
                {ok, ?AIR_OUTSIDE} ->
                    %% Remove, expand adjacent.
                    lists:foldl(Loop, maps:remove(Pos, ExplicitCubes), [
                        {X - 1, Y, Z},
                        {X + 1, Y, Z},
                        {X, Y - 1, Z},
                        {X, Y + 1, Z},
                        {X, Y, Z - 1},
                        {X, Y, Z + 1}
                    ]);
                {ok, ?LAVA} ->
                    ExplicitCubes;
                error ->
                    ExplicitCubes
            end
        end)(
            MinPos, ExplicitCubes0
        ),
    %% Rewrite the ?AIR_OUTSIDE voxels left as ?AIR_INSIDE.
    maps:map(
        fun
            (_, ?LAVA) -> ?LAVA;
            (_, ?AIR_OUTSIDE) -> ?AIR_INSIDE
        end,
        Cubes2
    ).
