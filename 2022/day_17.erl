-module(day_17).

%% -export([main/1]).
-compile(export_all).

-spec shape_1() -> map().
shape_1() -> #{{1, 1} => $#, {1, 2} => $#, {1, 3} => $#, {1, 4} => $#}.

-spec shape_2() -> map().
shape_2() -> #{{1, 2} => $#, {2, 1} => $#, {2, 2} => $#, {2, 3} => $#, {3, 2} => $#}.

-spec shape_3() -> map().
shape_3() -> #{{1, 3} => $#, {2, 3} => $#, {3, 1} => $#, {3, 2} => $#, {3, 3} => $#}.

-spec shape_4() -> map().
shape_4() -> #{{1, 1} => $#, {2, 1} => $#, {3, 1} => $#, {4, 1} => $#}.

-spec shape_5() -> map().
shape_5() -> #{{1, 1} => $#, {1, 2} => $#, {2, 1} => $#, {2, 2} => $#}.

-spec shapes() -> [map()].
shapes() -> [shape_1(), shape_2(), shape_3(), shape_4(), shape_5()].

-spec move_shape(grids:dir() | grids:pos(integer()), map()) -> map().
move_shape(Offset = {_, _}, Shape) ->
    maps:fold(
        fun(Pos, Char, Result) ->
            Result#{grids:add_pos(Pos, Offset) => Char}
        end,
        #{},
        Shape
    );
move_shape(Dir, Shape) ->
    move_shape(grids:dir_to_pos(Dir), Shape).

-spec main(1..2) -> ok.
main(1) ->
    Moves = [grids:char_to_dir(C) || <<C>> <= hd(io_ext:read_lines(standard_io, 1))],
    io:format("Moves ~p~n", [Moves]),
    io:format("Shapes ~p~n", [shapes()]),
    %% TODO
    ok.
