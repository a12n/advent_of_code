-module(io_ext).

-export([read_lines/1]).

-spec read_lines(io:device()) -> [binary()].
read_lines(File) ->
    case io:get_line(File, <<>>) of
        eof -> [];
        <<Line/bytes>> -> [string:trim(Line) | read_lines(File)]
    end.
