-module(day_17).

%% -export([main/1]).
-compile(export_all).

-type shape() :: #{grids:pos(integer()) => char()}.

-spec shape([string()]) -> shape().
shape(Definition) ->
    maps:from_list(
        [
            %% Shape's zero coordinate is at the bottom left
            %% corner. Reverse lines and negate row numbers.
            {{-Row, Col}, Char}
         || {Row, Line} <- lists:enumerate(0, lists:reverse(Definition)),
            {Col, Char} <- lists:enumerate(0, Line),
            Char =/= $\s,
            Char =/= $.
        ]
    ).

-spec shapes() -> [shape()].
shapes() ->
    [
        shape([
            "####"
        ]),
        shape([
            " # ",
            "###",
            " # "
        ]),
        shape([
            "  #",
            "  #",
            "###"
        ]),
        shape([
            "#",
            "#",
            "#",
            "#"
        ]),
        shape([
            "##",
            "##"
        ])
    ].

-spec move_shape(grids:dir() | grids:pos(integer()), shape()) -> shape().
move_shape(Move = {_, _}, Shape) ->
    maps:from_list(
        [{grids:add_pos(Pos, Move), Char} || {Pos, Char} <- maps:to_list(Shape)]
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
