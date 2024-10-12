-module(grids).

-type dir() :: up | down | left | right.
-type pos() :: pos(pos_integer()).
-type pos(Integer) :: {Integer, Integer}.
-type extent() :: extent(pos_integer()).
-type extent(Integer) :: {pos(Integer), pos(Integer)}.
-export_type([dir/0, pos/0, pos/1, extent/0, extent/1]).

-type grid(Integer, Value) :: #{pos(Integer) := Value}.
-type grid(Value) :: grid(pos_integer(), Value).
-export_type([grid/1, grid/2]).

-type format_opts() :: #{blank => char()}.
-export_type([format_opts/0]).

-export([
    add_pos/2,
    sub_pos/2,
    taxicab_distance/2,
    is_valid_pos/2,
    dir_to_pos/1,
    dir_to_pos/2,
    pos_to_dir/1,
    char_to_dir/1,
    dir_to_char/1,
    rotate_dir/2
]).

-export([
    from_lines/1,
    to_iodata/1, to_iodata/3,
    find_values/2,
    size/1,
    extent/1,
    extent_area/1,
    move/2,
    intersects/2,
    intersects_line/2
]).

-spec add_pos(pos(integer()), pos(integer())) -> pos(integer()).
add_pos({Row1, Col1}, {Row2, Col2}) -> {Row1 + Row2, Col1 + Col2}.

-spec sub_pos(pos(integer()), pos(integer())) -> pos(integer()).
sub_pos({Row1, Col1}, {Row2, Col2}) -> {Row1 - Row2, Col1 - Col2}.

-spec taxicab_distance(pos(integer()), pos(integer())) -> non_neg_integer().
taxicab_distance({Row1, Col1}, {Row2, Col2}) -> abs(Row1 - Row2) + abs(Col1 - Col2).

-spec is_valid_pos(pos(integer()), {pos(integer()), pos(integer())}) -> boolean().
is_valid_pos({Row, Col}, {{MinRow, MinCol}, {MaxRow, MaxCol}}) ->
    Row >= MinRow andalso Row =< MaxRow andalso
        Col >= MinCol andalso Col =< MaxCol.

-spec dir_to_pos(dir()) -> pos(-1..1).
dir_to_pos(Dir) -> dir_to_pos(Dir, 1).

-spec dir_to_pos(dir(), pos_integer()) -> pos(integer()).
dir_to_pos(up, N) -> {-N, 0};
dir_to_pos(left, N) -> {0, -N};
dir_to_pos(right, N) -> {0, N};
dir_to_pos(down, N) -> {N, 0}.

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

-spec dir_to_char(dir()) -> char().
dir_to_char(up) -> $↑;
dir_to_char(left) -> $←;
dir_to_char(right) -> $→;
dir_to_char(down) -> $↓.

-spec rotate_dir(cw | ccw, dir()) -> dir().
rotate_dir(cw, up) -> right;
rotate_dir(cw, right) -> down;
rotate_dir(cw, down) -> left;
rotate_dir(cw, left) -> up;
rotate_dir(ccw, up) -> left;
rotate_dir(ccw, right) -> up;
rotate_dir(ccw, down) -> right;
rotate_dir(ccw, left) -> down.

-spec from_lines([iodata()]) -> grid(char()).
from_lines(Lines) ->
    maps:from_list(
        [
            {{Row, Col}, Value}
         || {Row, Line} <- lists:enumerate(Lines),
            {Col, Value} <- lists:enumerate(binary_to_list(iolist_to_binary(Line)))
        ]
    ).

-spec to_iodata(grid(integer(), char())) -> iodata().
to_iodata(Grid) -> to_iodata(Grid, extent(Grid), #{}).

-spec to_iodata(grid(integer(), char()), extent(integer()), format_opts()) ->
    iodata().
to_iodata(Grid, {{MinRow, MinCol}, {MaxRow, MaxCol}}, Opts) ->
    Blank = maps:get(blank, Opts, $\s),
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

-spec size(grid(term())) -> pos(non_neg_integer()).
size(Grid) ->
    maps:fold(
        fun({Row, Col}, _, {NumRows, NumCols}) ->
            {max(NumRows, Row), max(NumCols, Col)}
        end,
        {0, 0},
        Grid
    ).

-spec extent(grid(integer(), term())) -> extent(integer()) | undefined.
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

-spec extent_area(extent(integer())) -> non_neg_integer().
extent_area({{MinRow, MinCol}, {MaxRow, MaxCol}}) -> (MaxRow - MinRow + 1) * (MaxCol - MinCol + 1).

-spec move(dir() | pos(integer()), grid(integer(), term())) -> grid(integer(), term()).
move(Move = {_, _}, Grid) ->
    maps:from_list(
        [{grids:add_pos(Pos, Move), Value} || {Pos, Value} <- maps:to_list(Grid)]
    );
move(Dir, Grid) ->
    move(dir_to_pos(Dir), Grid).

-spec intersects(grid(integer(), term()), grid(integer(), term())) -> boolean().
intersects(Grid1, Grid2) ->
    try
        maps:intersect_with(fun(Pos, _, _) -> throw(Pos) end, Grid1, Grid2),
        false
    catch
        throw:{_, _} ->
            true
    end.

-spec intersects_line(grid(integer(), term()), {vert | horiz, integer()}) -> boolean().
intersects_line(Grid, {vert, Line}) -> maps_ext:any(fun({_, Col}, _) -> Col == Line end, Grid);
intersects_line(Grid, {horiz, Line}) -> maps_ext:any(fun({Row, _}, _) -> Row == Line end, Grid).
