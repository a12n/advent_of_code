-module(advent).

-export([file_lines/1]).

-spec file_lines(io:device()) -> [binary()].
file_lines(File) ->
    case io:get_line(File, <<>>) of
        eof -> [];
        <<Line/bytes>> -> [string:trim(Line) | file_lines(File)]
    end.
