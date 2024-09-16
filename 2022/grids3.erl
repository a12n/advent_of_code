-module(grids3).

-type pos() :: pos(pos_integer()).
-type pos(Integer) :: {Integer, Integer, Integer}.
-export_type([pos/0, pos/1]).

-type grid(Integer, Value) :: #{pos(Integer) := Value}.
-type grid(Value) :: grid(pos_integer(), Value).
-export_type([grid/1, grid/2]).

-export([
    add_pos/2,
    sub_pos/2,
    taxicab_distance/2,
    is_valid_pos/3,
    extent/1
]).

-spec add_pos(pos(integer()), pos(integer())) -> pos(integer()).
add_pos({X1, Y1, Z1}, {X2, Y2, Z2}) -> {X1 + X2, Y1 + Y2, Z1 + Z2}.

-spec sub_pos(pos(integer()), pos(integer())) -> pos(integer()).
sub_pos({X1, Y1, Z1}, {X2, Y2, Z2}) -> {X1 - X2, Y1 - Y2, Z1 - Z2}.

-spec taxicab_distance(pos(integer()), pos(integer())) -> non_neg_integer().
taxicab_distance({X1, Y1, Z1}, {X2, Y2, Z2}) -> abs(X1 - X2) + abs(Y1 - Y2) + abs(Z1 - Z2).

-spec is_valid_pos(pos(integer()), pos(integer()), pos(integer())) -> boolean().
is_valid_pos({X, Y, Z}, {MinX, MinY, MinZ}, {MaxX, MaxY, MaxZ}) ->
    X >= MinX andalso X =< MaxX andalso
        Y >= MinY andalso Y =< MaxY andalso
        Z >= MinZ andalso Z =< MaxZ.

-spec extent(grid(integer(), term())) -> {pos(integer()), pos(integer())} | undefined.
extent(Grid) ->
    maps:fold(
        fun
            ({X, Y, Z}, _, {{MinX, MinY, MinZ}, {MaxX, MaxY, MaxZ}}) ->
                {
                    {min(MinX, X), min(MinY, Y), min(MinZ, Z)},
                    {max(MaxX, X), max(MaxY, Y), max(MaxZ, Z)}
                };
            (Pos, _, undefined) ->
                {Pos, Pos}
        end,
        undefined,
        Grid
    ).
