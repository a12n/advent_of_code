-module(grid_position).

-type direction() :: up | down | left | right.
-type t() :: t(pos_integer()).
-type t(Integer) :: {Integer, Integer}.
-export_type([direction/0, t/0, t/1]).

-export([add/2, from_direction/1, to_direction/1, transpose/1]).

-spec add(t(integer()), t(integer())) -> t(integer()).
add({Row1, Col1}, {Row2, Col2}) -> {Row1 + Row2, Col1 + Col2}.

-spec from_direction(direction()) -> t(-1..1).
from_direction(up) -> {-1, 0};
from_direction(left) -> {0, -1};
from_direction(right) -> {0, 1};
from_direction(down) -> {1, 0}.

-spec to_direction(t(integer())) -> direction().
to_direction({Row, 0}) when Row < 0 -> up;
to_direction({Row, 0}) when Row > 0 -> down;
to_direction({0, Col}) when Col < 0 -> left;
to_direction({0, Col}) when Col > 0 -> right.

-spec transpose(t(integer())) -> t(integer()).
transpose({Row, Col}) -> {Col, Row}.
