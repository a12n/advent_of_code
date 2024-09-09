-module(day_12).

-include_lib("eunit/include/eunit.hrl").

-export([main/1]).

-spec main(1..2) -> ok.
main(1) ->
    HeightMap = maps:map(
        fun
            (_, $S) -> $a - 1;
            (_, $E) -> $z + 1;
            (_, C) -> C
        end,
        grids:from_lines(io_ext:read_lines(standard_io))
    ),
    HeightMapSize = grids:size(HeightMap),
    io:format(grids:to_iodata(HeightMap, HeightMapSize)),
    ok.
