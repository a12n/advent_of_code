-module(day_18).

-export([main/1]).

-spec main(1..2) -> ok.
main(1) ->
    CubeMap =
        maps:from_list(
            lists:map(
                fun(Pos) ->
                    {Pos, undefined}
                end,
                lists:map(
                    fun(<<Line/bytes>>) ->
                        [X, Y, Z] = binary:split(Line, <<",">>, [global, trim]),
                        {binary_to_integer(X), binary_to_integer(Y), binary_to_integer(Z)}
                    end,
                    io_ext:read_lines(standard_io)
                )
            )
        ),
    NumExposedMap = maps:map(
        fun({X, Y, Z}, _) ->
            length([
                AdjPos
             || AdjPos <- [
                    {X - 1, Y, Z},
                    {X + 1, Y, Z},
                    {X, Y - 1, Z},
                    {X, Y + 1, Z},
                    {X, Y, Z - 1},
                    {X, Y, Z + 1}
                ],
                not maps:is_key(AdjPos, CubeMap)
            ])
        end,
        CubeMap
    ),
    NumExposed = lists:sum(maps:values(NumExposedMap)),
    io:format(<<"~b~n">>, [NumExposed]).
