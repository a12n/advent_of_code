-module(day_10).

-export([main/1]).

-spec main(1..2) -> ok.
main(1) ->
    Program = lists:map(
        fun
            (<<"addx ", N/bytes>>) -> {addx, binary_to_integer(N)};
            (<<"noop">>) -> noop
        end,
        io_ext:read_lines(standard_io)
    ),
    io:format("~p~n", [Program]),
    %% TODO
    ok.
