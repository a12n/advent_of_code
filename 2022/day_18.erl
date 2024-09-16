-module(day_18).

-export([main/1]).

-define(AIR, $.).
-define(LAVA, $#).

-spec main(1..2) -> ok.
main(1) ->
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
    io:format(<<"~b~n">>, [surface_area(Cubes)]).

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
