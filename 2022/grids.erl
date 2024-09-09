-module(grids).

-type dir() :: up | down | left | right.
-type pos() :: pos(pos_integer()).
-type pos(Integer) :: {Integer, Integer}.
-export_type([dir/0, pos/0, pos/1]).

-type grid(Value) :: #{pos() := Value}.
-export_type([grid/1]).

-export([add_pos/2, sub_pos/2, dir_to_pos/1, pos_to_dir/1]).
-export([from_lines/1, transpose/1, size/1]).

-spec add_pos(pos(integer()), pos(integer())) -> pos(integer()).
add_pos({Row1, Col1}, {Row2, Col2}) -> {Row1 + Row2, Col1 + Col2}.

-spec sub_pos(pos(integer()), pos(integer())) -> pos(integer()).
sub_pos({Row1, Col1}, {Row2, Col2}) -> {Row1 - Row2, Col1 - Col2}.

-spec dir_to_pos(dir()) -> pos(-1..1).
dir_to_pos(up) -> {-1, 0};
dir_to_pos(left) -> {0, -1};
dir_to_pos(right) -> {0, 1};
dir_to_pos(down) -> {1, 0}.

-spec pos_to_dir(pos(integer())) -> dir().
pos_to_dir({Row, 0}) when Row < 0 -> up;
pos_to_dir({Row, 0}) when Row > 0 -> down;
pos_to_dir({0, Col}) when Col < 0 -> left;
pos_to_dir({0, Col}) when Col > 0 -> right.

-spec from_lines([iodata()]) -> grid(char()).
from_lines(Lines) ->
    maps:from_list(
        [
            {{Row, Col}, Value}
         || {Row, Line} <- lists:enumerate(Lines),
            {Col, Value} <- lists:enumerate(binary_to_list(iolist_to_binary(Line)))
        ]
    ).

-spec transpose(grid(term())) -> grid(term()).
transpose(Grid) ->
    maps:fold(
        fun(Pos, Value, Result) ->
            maps:update(grid_pos:transpose(Pos), Value, Result)
        end,
        #{},
        Grid
    ).

-spec size(grid(term())) -> {non_neg_integer(), non_neg_integer()}.
size(Grid) ->
    maps:fold(
        fun({Row, Col}, _, {NumRows, NumCols}) ->
            {max(NumRows, Row), max(NumCols, Col)}
        end,
        {0, 0},
        Grid
    ).
