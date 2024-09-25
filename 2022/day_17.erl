-module(day_17).

-include_lib("eunit/include/eunit.hrl").

%% -export([main/1]).
-compile(export_all).

-type shape() :: grids:grid(integer(), char()).

-type shape_bits() :: 2#0000000..2#1111111.
-type shape2() :: nonempty_list(shape_bits()).

-spec main(1..2) -> ok.
main(1) ->
    Shifts = lazy_lists:cycle(
        infinity,
        lazy_lists:from_list([
            grids:char_to_dir(Char)
         || Line <- io_ext:read_lines(standard_io, 1), Char <- binary_to_list(Line)
        ])
    ),
    Shapes = lazy_lists:cycle(infinity, lazy_lists:from_list(shapes2())),
    Ground2 = simulate2(Shapes, Shifts, _Ground = [], _N = 2022),
    io:format(<<"~b~n">>, [length(Ground2)]).

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

-spec shapes2() -> [shape2()].
shapes2() ->
    [
        %% ####
        [
            2#0011110
        ],
        %% .#.
        %% ###
        %% .#.
        [
            2#0001000,
            2#0011100,
            2#0001000
        ],
        %% ..#
        %% ..#
        %% ###
        [
            2#0000100,
            2#0000100,
            2#0011100
        ],
        %% #
        %% #
        %% #
        %% #
        [
            2#0010000,
            2#0010000,
            2#0010000,
            2#0010000
        ],
        %% ##
        %% ##
        [
            2#0011000,
            2#0011000
        ]
    ].

-spec shape_bits_to_iodata(shape_bits()) -> iodata().
shape_bits_to_iodata(Bits) ->
    [
        case (Bits bsr K) band 1 of
            0 -> $.;
            1 -> $#
        end
     || K <- lists:seq(6, 0, -1)
    ].

-spec shape_to_iodata(shape2()) -> iodata().
shape_to_iodata(Shape) -> [[shape_bits_to_iodata(Bits), $\n] || Bits <- Shape].

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

-spec shift_bits(shape_bits(), left | right) -> shape_bits().
shift_bits(Bits, left) when (Bits band 2#1000000) == 0 -> Bits bsl 1;
shift_bits(Bits, right) when (Bits band 2#0000001) == 0 -> Bits bsr 1.

-spec shift(shape2(), left | right) -> shape2().
shift(Shape, Dir) ->
    try
        [shift_bits(Bits, Dir) || Bits <- Shape]
    catch
        error:function_clause -> Shape
    end.

-spec intersects(shape2(), shape2()) -> boolean().
intersects([], _) ->
    false;
intersects(_, []) ->
    false;
intersects([Bits1 | Shape1], [Bits2 | Shape2]) ->
    case Bits1 band Bits2 of
        0 -> intersects(Shape1, Shape2);
        _ -> true
    end.

-spec merge_shapes(shape2(), shape2()) -> shape2().
merge_shapes(Shape1, []) ->
    Shape1;
merge_shapes([], Shape2) ->
    Shape2;
merge_shapes([Bits1 | Shape1], [Bits2 | Shape2]) ->
    [(Bits1 bor Bits2) | merge_shapes(Shape1, Shape2)].

-spec simulate_one2(shape2(), shape2(), lazy_lists:lazy_list(left | right)) ->
    {shape2(), lazy_list:lazy_list(left | right)}.
simulate_one2(Shape, Ground, [Dir | Shifts]) ->
    io:format(standard_error, <<"Dir ~p~n">>, [Dir]),
    io:format(standard_error, <<"~s~n">>, [shape_to_iodata(merge_shapes(Shape, Ground))]),
    %% Push to the side.
    Shape2 = shift(Shape, Dir),
    %% If push makes it intersect with the ground, undo the push.
    Shape3 =
        case intersects(Shape2, Ground) of
            false -> Shape2;
            true -> Shape
        end,
    %% Move down, intersect with the next ground level.
    case Ground of
        [] ->
            %% Reached bottom, the shape is the new ground now.
            {Shape3, lazy_lists:force_tail(Shifts)};
        [FirstGroundBits | NextGround] ->
            %% Check intersection with the next ground level.
            case intersects(Shape3, NextGround) of
                false ->
                    %% Doesn't intersect, fall further.
                    {Ground2, Shifts2} = simulate_one2(
                        Shape3, NextGround, lazy_lists:force_tail(Shifts)
                    ),
                    {[FirstGroundBits | Ground2], Shifts2};
                true ->
                    Ground2 = merge_shapes(Shape3, Ground),
                    {Ground2, lazy_lists:force_tail(Shifts)}
            end
    end.

-spec simulate2(
    lazy_lists:lazy_list(shape2()),
    lazy_lists:lazy_list(left | right),
    shape2(),
    non_neg_integer()
) -> shape2().
simulate2(_, _, Ground, 0) ->
    Ground;
simulate2([Shape | Shapes], Shifts, Ground, N) ->
    %% Extend ground up with empty bits, to align with the falling shape.
    Ground2 = lists:duplicate(length(Shape) + 3, 2#0000000) ++ Ground,
    %% Fall shape.
    {Ground3, Shifts2} = simulate_one2(Shape, Ground2, Shifts),
    %% Remove empty bits from the top of the ground.
    Ground4 = lists:dropwhile(fun(Bits) -> Bits == 2#0000000 end, Ground3),
    %% Simulate next shape.
    simulate2(lazy_lists:force_tail(Shapes), Shifts2, Ground4, N - 1).

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
