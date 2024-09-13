-module(grids).

-type dir() :: up | down | left | right.
-type pos() :: pos(pos_integer()).
-type pos(Integer) :: {Integer, Integer}.
-export_type([dir/0, pos/0, pos/1]).

-type grid(Integer, Value) :: #{pos(Integer) := Value}.
-type grid(Value) :: grid(pos_integer(), Value).
-export_type([grid/1, grid/2]).

-export([
    add_pos/2,
    sub_pos/2,
    taxicab_distance/2,
    is_valid_pos/2,
    dir_to_pos/1,
    pos_to_dir/1,
    char_to_dir/1
]).

-export([
    from_lines/1,
    to_iodata/2, to_iodata/3,
    find_values/2,
    transpose/1,
    size/1,
    extent/1
]).

-spec add_pos(pos(integer()), pos(integer())) -> pos(integer()).
add_pos({Row1, Col1}, {Row2, Col2}) -> {Row1 + Row2, Col1 + Col2}.

-spec sub_pos(pos(integer()), pos(integer())) -> pos(integer()).
sub_pos({Row1, Col1}, {Row2, Col2}) -> {Row1 - Row2, Col1 - Col2}.

-spec taxicab_distance(pos(integer()), pos(integer())) -> non_neg_integer().
taxicab_distance({Row1, Col1}, {Row2, Col2}) -> abs(Row1 - Row2) + abs(Col1 - Col2).

-spec is_valid_pos(pos(), {non_neg_integer(), non_neg_integer()}) -> boolean().
is_valid_pos({Row, Col}, {NumRows, NumCols}) ->
    Row >= 1 andalso Row =< NumRows andalso
        Col >= 1 andalso Col =< NumCols.

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

-spec char_to_dir(char()) -> dir().
char_to_dir($^) -> up;
char_to_dir($v) -> down;
char_to_dir($<) -> left;
char_to_dir($>) -> right;
char_to_dir($U) -> up;
char_to_dir($D) -> down;
char_to_dir($L) -> left;
char_to_dir($R) -> right.

-spec from_lines([iodata()]) -> grid(char()).
from_lines(Lines) ->
    maps:from_list(
        [
            {{Row, Col}, Value}
         || {Row, Line} <- lists:enumerate(Lines),
            {Col, Value} <- lists:enumerate(binary_to_list(iolist_to_binary(Line)))
        ]
    ).

-spec to_iodata(grid(char()), {pos_integer(), pos_integer()}) -> iodata().
to_iodata(Grid, Size) -> to_iodata(Grid, _Min = {1, 1}, _Max = Size).

-spec to_iodata(grid(integer(), char()), pos(integer()), pos(integer())) -> iodata().
to_iodata(Grid, {MinRow, MinCol}, {MaxRow, MaxCol}) ->
    Blank = $.,
    (fun
        Loop(Row, _) when Row == MaxRow + 1 -> "";
        Loop(Row, Col) when Col == MaxCol + 1 ->
            [$\n | Loop(Row + 1, MinCol)];
        Loop(Row, Col) ->
            [maps:get({Row, Col}, Grid, Blank) | Loop(Row, Col + 1)]
    end)(
        MinRow, MinCol
    ).

-spec find_values(term(), grid(term())) -> [pos()].
find_values(Query, Grid) ->
    maps:fold(
        fun
            (Pos, Value, List) when Value == Query -> [Pos | List];
            (_, _, List) -> List
        end,
        [],
        Grid
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

-spec size(grid(term())) -> pos(non_neg_integer()).
size(Grid) ->
    maps:fold(
        fun({Row, Col}, _, {NumRows, NumCols}) ->
            {max(NumRows, Row), max(NumCols, Col)}
        end,
        {0, 0},
        Grid
    ).

-spec extent(grid(integer(), term())) -> {pos(integer()), pos(integer())} | undefined.
extent(Grid) ->
    maps:fold(
        fun
            ({Row, Col}, _, {{MinRow, MinCol}, {MaxRow, MaxCol}}) ->
                {
                    {min(MinRow, Row), min(MinCol, Col)},
                    {max(MaxRow, Row), max(MaxCol, Col)}
                };
            (Pos, _, undefined) ->
                {Pos, Pos}
        end,
        undefined,
        Grid
    ).
