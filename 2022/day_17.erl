-module(day_17).

-include_lib("eunit/include/eunit.hrl").

-export([main/1]).

-type shape_bits() :: 2#0000000..2#1111111.
-type shape2() :: nonempty_list(shape_bits()).

-spec main(1..2) -> ok.
main(Part) ->
    Shifts = list_to_tuple([
        grids:char_to_dir(Char)
     || Line <- io_ext:read_lines(standard_io, 1),
        Char <- binary_to_list(Line)
    ]),
    N =
        case Part of
            1 -> 2022;
            2 -> 1000000000000
        end,
    Shapes = list_to_tuple(shapes2()),
    Height = simulate2(Shapes, 0, Shifts, 0, _Ground = [], N, 0),
    io:format(<<"~b~n">>, [Height]).

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

-spec simulate_one2(shape2(), tuple(), non_neg_integer(), shape2()) ->
    {shape2(), non_neg_integer()}.
simulate_one2(Shape, ShiftArray, ShiftIndex, Ground) ->
    Dir = element(ShiftIndex + 1, ShiftArray),
    ShiftIndex2 = (ShiftIndex + 1) rem size(ShiftArray),
    %% Push to the side.
    Shape2 = shift(Shape, Dir),
    %% If push makes it intersect with the ground, undo the push.
    Shape3 =
        case intersects(Shape2, Ground) of
            false -> Shape2;
            true -> Shape
        end,
    %% Move down, intersect with the next ground level.
    %% Check intersection with the next ground level.
    case {Shape3, Ground} of
        {[_], [_]} ->
            {merge_shapes(Shape3, Ground), ShiftIndex2};
        {[_, _], [_, _]} ->
            {merge_shapes(Shape3, Ground), ShiftIndex2};
        {[_, _, _], [_, _, _]} ->
            {merge_shapes(Shape3, Ground), ShiftIndex2};
        {[_, _, _, _], [_, _, _, _]} ->
            {merge_shapes(Shape3, Ground), ShiftIndex2};
        {_, [GroundBits1 | NextGround]} ->
            case intersects(Shape3, NextGround) of
                false ->
                    %% Doesn't intersect, fall further.
                    {Ground2, ShiftIndex3} = simulate_one2(
                        Shape3, ShiftArray, ShiftIndex2, NextGround
                    ),
                    {[GroundBits1 | Ground2], ShiftIndex3};
                true ->
                    {merge_shapes(Shape3, Ground), ShiftIndex2}
            end
    end.

-spec simulate2(
    tuple(),
    non_neg_integer(),
    tuple(),
    non_neg_integer(),
    shape2(),
    pos_integer(),
    non_neg_integer()
) -> shape2().
simulate2(_, _, _, _, Ground, N, I) when I >= N ->
    length(Ground);
simulate2(ShapeArray, ShapeIndex, ShiftArray, ShiftIndex, Ground, N, I) ->
    Key = {ShapeIndex, ShiftIndex, lists_ext:take(10000, Ground)},
    case persistent_term:get(Key, undefined) of
        {CycleI, CycleLen} when ((N - I) div CycleI) > 0 ->
            Times = (N - I) div CycleI,
            io:format(
                standard_error,
                <<"Already seen: Shape ~b, Shift ~b, Ground ~p, CycleI ~p, CycleLen ~p, Times ~p~n">>,
                [
                    element(1, Key),
                    element(2, Key),
                    lists_ext:take(5, element(3, Key)),
                    CycleI,
                    CycleLen,
                    Times
                ]
            ),
            %% TODO: simulate2(Shapes, NextShiftsAfterSimulateOne, Ground, Height + Times * CycleLen, I + Times * CycleI)
            ok;
        _ ->
            ok
        %% error({already_seen, Key, {J, Len}})
    end,
    %% Extend ground up with empty bits, to align with the falling shape.
    Shape = element(ShapeIndex + 1, ShapeArray),
    ShapeIndex2 = (ShapeIndex + 1) rem size(ShapeArray),
    Ground2 = lists:duplicate(length(Shape) + 3, 2#0000000) ++ Ground,
    %% Fall shape.
    {Ground3, ShiftIndex2} = simulate_one2(Shape, ShiftArray, ShiftIndex, Ground2),
    %% Remove empty bits from the top of the ground.
    Ground4 = lists:dropwhile(fun(Bits) -> Bits == 2#0000000 end, Ground3),
    %% Save already seen state for cycle detection.
    ok = persistent_term:put(Key, {I, length(Ground4)}),
    %% Simulate next shape.
    simulate2(ShapeArray, ShapeIndex2, ShiftArray, ShiftIndex2, Ground4, N, I + 1).
