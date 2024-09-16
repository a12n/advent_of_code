-module(day_18).

-export([main/1]).

-define(AIR, $.).
-define(LAVA, $#).

-spec main(1..2) -> ok.
main(Part) ->
    Cubes =
        maps:from_list(
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
    case Part of
        1 ->
            io:format(<<"~b~n">>, [surface_area(Cubes)]);
        2 ->
            {MinPos, MaxPos} = grids3:extent(Cubes),
            io:format(<<"MinPos ~p, MaxPos ~p~n">>, [MinPos, MaxPos]),
            ok
    end.

-spec surface_area(map()) -> non_neg_integer().
surface_area(Cubes) ->
    length([
        AdjPos
     || {{X, Y, Z}, ?LAVA} <- maps:to_list(Cubes),
        AdjPos <- [
            {X - 1, Y, Z},
            {X + 1, Y, Z},
            {X, Y - 1, Z},
            {X, Y + 1, Z},
            {X, Y, Z - 1},
            {X, Y, Z + 1}
        ],
        maps:get(AdjPos, Cubes, ?AIR) /= ?LAVA
    ]).
