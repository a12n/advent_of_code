-module(io_ext).

-export([read_lines/1, read_lines/2]).

-spec read_lines(io:device()) -> [binary()].
read_lines(File) -> read_lines(File, -1).

-spec read_lines(io:device(), integer()) -> [binary()].
read_lines(_File, 0) ->
    [];
read_lines(File, N) ->
    case io:get_line(File, <<>>) of
        eof ->
            [];
        <<Line/bytes>> ->
            [string:trim(Line) | read_lines(File, N - 1)]
    end.
