-module(day_17).

-include_lib("eunit/include/eunit.hrl").

%% -export([main/1]).
-compile(export_all).

-type shape() :: grids:grid(integer(), char()).

-spec main(1..2) -> ok.
main(1) ->
    MovesSeq = lazy_lists:cycle(
        infinity,
        lazy_lists:from_list([
            grids:char_to_dir(Char)
         || Line <- io_ext:read_lines(standard_io, 1), Char <- binary_to_list(Line)
        ])
    ),
    ShapesSeq = lazy_lists:cycle(infinity, lazy_lists:from_list(shapes())),
    LeftWall = 0 - (2 + 1),
    RightWall = LeftWall + (7 + 1),
    Ground = maps:from_list([{{0, Col}, $-} || Col <- lists:lazy_list(LeftWall + 1, RightWall - 1)]),
    %% ?debugFmt("LeftWall ~p, RightWall ~p", [LeftWall, RightWall]),
    %% ?debugFmt("Ground ~p", [Ground]),
    Ground2 = simulate(ShapesSeq, MovesSeq, LeftWall, RightWall, Ground, _NumShapes = 2022),
    io:format(<<"~b~n">>, [-top(Ground2)]).

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

-spec push_side(shape(), left | right, integer(), integer()) -> shape().
push_side(Shape, Move, LeftWall, RightWall) ->
    Shape2 = grids:move(Move, Shape),
    case
        grids:intersects_line(Shape2, {vert, LeftWall}) orelse
            grids:intersects_line(Shape2, {vert, RightWall})
    of
        false -> Shape2;
        true -> Shape
    end.

-spec fall_down(shape(), shape()) -> {true, shape()} | false.
fall_down(Shape, Ground) ->
    Shape2 = grids:move(down, Shape),
    case grids:intersects(Shape2, Ground) of
        false -> {true, Shape2};
        true -> false
    end.

-spec simulate_one(shape(), lazy_lists:lazy_list(grids:dir()), integer(), integer(), shape()) ->
    {shape(), lazy_lists:lazy_list(grids:dir())}.
simulate_one(Shape0, Moves0, LeftWall, RightWall, Ground) ->
    (fun Loop(Shape, Moves) ->
        {Move, NextMoves} = Moves(),
        %% ?debugFmt("Move ~p, Shape~n~s", [Move, grids:to_iodata(maps:merge(Shape, Ground))]),
        Shape2 = grids:move(Move, Shape),
        %% ?debugFmt("Shape2 ~p", [Shape2]),
        %% Pushed by a jet of hot gas one unit.
        Shape3 =
            case
                grids:intersects_line(Shape2, {vert, LeftWall}) orelse
                    grids:intersects_line(Shape2, {vert, RightWall}) orelse
                    grids:intersects(Shape2, Ground)
            of
                true ->
                    %% Moved shape intersects with the walls, revert the move.
                    %% ?debugFmt("Intersects with walls [~p, ~p], revert move", [LeftWall, RightWall]),
                    Shape;
                false ->
                    %% Don't intersect with the walls, the shape may be moved.
                    %% ?debugFmt("Doesn't intersect with walls [~p, ~p]", [LeftWall, RightWall]),
                    Shape2
            end,
        %% Fall one unit down.
        Shape4 = grids:move(down, Shape3),
        %% ?debugFmt("After move down ~p", [Shape4]),
        case grids:intersects(Shape4, Ground) of
            true ->
                %% Moved shape intersects with the ground, revert
                %% the move. Shape is on the ground, it's a part
                %% of ground now. Return the updated ground.
                %% ?debugFmt("Can't move down, merge ~p with the ground", [Shape3]),
                {maps:merge(Ground, Shape3), NextMoves};
            false ->
                %% Shape is still falling, next step of the
                %% simulation.
                %% ?debugFmt("Moved down, the shape now ~p", [Shape4]),
                Loop(Shape4, NextMoves)
        end
    end)(
        Shape0, Moves0
    ).

-spec simulate(
    lazy_lists:lazy_list(shape()),
    lazy_lists:lazy_list(grids:dir()),
    integer(),
    integer(),
    shape(),
    non_neg_integer()
) -> shape().
simulate(Shapes0, Moves0, LeftWall, RightWall, Ground0, NumShapes0) ->
    (fun
        Loop(_, _, Ground, 0) ->
            %% ?debugFmt("NumShapes ~p, return ground ~p", [0, Ground]),
            Ground;
        Loop(Shapes, Moves, Ground, NumShapes) ->
            {Shape0, NextShapes} = Shapes(),
            Shape = grids:move({top(Ground) - (3 + 1), 0}, Shape0),
            %% TODO: Move shape to the row.
            %% ?debugFmt("Shape ~p, NumShapes ~p", [Shape, NumShapes]),
            %% ?debugFmt("Ground ~p", [Ground]),
            {Ground2, NextMoves} = simulate_one(Shape, Moves, LeftWall, RightWall, Ground),
            %% ?debugFmt("Ground2~n~s", [grids:to_iodata(Ground2)]),
            Loop(NextShapes, NextMoves, Ground2, NumShapes - 1)
    end)(
        Shapes0, Moves0, Ground0, NumShapes0
    ).

-spec top(shape()) -> integer().
top(Shape) -> maps:fold(fun({Row, _}, _, Min) -> min(Min, Row) end, infinity, Shape).
