-module(map_grids).

-type t(Value) :: #{grid_position:t() := Value}.
-export_type([t/1]).

-export([from_lines/1, transpose/1]).

-spec from_lines([iodata()]) -> t(char()).
from_lines(Lines) ->
    maps:from_list(
        [
            {{Row, Col}, Value}
         || {Row, Line} <- lists:enumerate(Lines),
            {Col, Value} <- lists:enumerate(binary_to_list(iolist_to_binary(Line)))
        ]
    ).

-spec transpose(t(term())) -> t(term()).
transpose(Grid) ->
    maps:fold(
        fun(Pos, Value, Result) ->
            maps:update(grid_pos:transpose(Pos), Value, Result)
        end,
        #{},
        Grid
    ).
