-module(day_17).

-include_lib("eunit/include/eunit.hrl").

-export([main/1]).

-type shape() :: nonempty_list(shape_bits()).
-type shape_bits() :: 2#0000000..2#1111111.

-spec main(1..2) -> ok.
main(Part) ->
    _Table = ets:new(cycles, [bag, named_table]),
    Shifts = list_to_tuple([
        grids:char_to_dir(Char)
     || Line <- io_ext:read_lines(standard_io, 1),
        <<Char>> <= Line
    ]),
    N =
        case Part of
            1 -> 2022;
            2 -> 1000000000000
        end,
    Height = simulate(list_to_tuple(shapes()), 0, Shifts, 0, _Ground = [], _Height = 0, N),
    io:format(<<"~b~n">>, [Height]).

-spec shapes() -> [shape()].
shapes() ->
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

%%--------------------------------------------------------------------
%% Shape bits operations
%%--------------------------------------------------------------------

-spec shift_bits(shape_bits(), left | right) -> shape_bits().
shift_bits(Bits, left) when (Bits band 2#1000000) == 0 -> Bits bsl 1;
shift_bits(Bits, right) when (Bits band 2#0000001) == 0 -> Bits bsr 1.

-spec shift(shape(), left | right) -> shape().
shift(Shape, Dir) ->
    try
        [shift_bits(Bits, Dir) || Bits <- Shape]
    catch
        error:function_clause -> Shape
    end.

-spec intersects(shape(), shape()) -> boolean().
intersects([], _) ->
    false;
intersects(_, []) ->
    false;
intersects([Bits1 | Shape1], [Bits2 | Shape2]) ->
    case Bits1 band Bits2 of
        0 -> intersects(Shape1, Shape2);
        _ -> true
    end.

-spec merge(shape(), shape()) -> shape().
merge(Shape1, []) -> Shape1;
merge([], Shape2) -> Shape2;
merge([Bits1 | Shape1], [Bits2 | Shape2]) -> [(Bits1 bor Bits2) | merge(Shape1, Shape2)].

%%--------------------------------------------------------------------
%% Simulation functions
%%--------------------------------------------------------------------

-spec simulate1(shape(), tuple(), non_neg_integer(), shape()) ->
    {shape(), non_neg_integer()}.
simulate1(Shape, ShiftArray, ShiftIndex, Ground) ->
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
            {merge(Shape3, Ground), ShiftIndex2};
        {[_, _], [_, _]} ->
            {merge(Shape3, Ground), ShiftIndex2};
        {[_, _, _], [_, _, _]} ->
            {merge(Shape3, Ground), ShiftIndex2};
        {[_, _, _, _], [_, _, _, _]} ->
            {merge(Shape3, Ground), ShiftIndex2};
        {_, [GroundBits1 | NextGround]} ->
            case intersects(Shape3, NextGround) of
                false ->
                    %% Doesn't intersect, fall further.
                    {Ground2, ShiftIndex3} = simulate1(
                        Shape3, ShiftArray, ShiftIndex2, NextGround
                    ),
                    {[GroundBits1 | Ground2], ShiftIndex3};
                true ->
                    {merge(Shape3, Ground), ShiftIndex2}
            end
    end.

-spec simulate(
    tuple(),
    non_neg_integer(),
    tuple(),
    non_neg_integer(),
    shape(),
    non_neg_integer(),
    non_neg_integer()
) -> non_neg_integer().
simulate(_, _, _, _, Ground, Height, 0) ->
    Height + length(Ground);
simulate(ShapeArray, ShapeIndex, ShiftArray, ShiftIndex, Ground, Height, N) ->
    Shape = element(ShapeIndex + 1, ShapeArray),
    ShapeIndex2 = (ShapeIndex + 1) rem size(ShapeArray),
    io:format(
        standard_error,
        <<"~b: ShapeIndex ~p, ShiftIndex ~p, Height ~p~n">>,
        [N, ShapeIndex, ShiftIndex, Height]
    ),
    Key = {ShapeIndex, ShiftIndex, lists_ext:take(1000, Ground)},
    case lists:sort(fun({_, {N1, _}}, {_, {N2, _}}) -> N1 >= N2 end, ets:lookup(cycles, Key)) of
        [
            {_, {CycleN1, CycleLength1}},
            {_, {CycleN2, CycleLength2}}
            | _
        ] when N > (CycleN1 - CycleN2) ->
            CycleN = CycleN1 - CycleN2,
            CycleLength = CycleLength2 - CycleLength1,
            Times = N div CycleN,
            io:format(
                standard_error,
                <<"Cycle: N ~p, CycleN ~p/~p/~p, CycleLength ~p/~p/~p, Times ~p~n">>,
                [N, CycleN1, CycleN2, CycleN, CycleLength1, CycleLength2, CycleLength, Times]
            ),
            simulate(
                ShapeArray,
                ShapeIndex,
                ShiftArray,
                ShiftIndex,
                Ground,
                Height + Times * CycleLength,
                N - Times * CycleN
            );
        _ ->
            %% Extend ground up with empty bits, to align with the falling shape.
            Ground2 = lists:duplicate(length(Shape) + 3, 2#0000000) ++ Ground,
            %% Fall shape.
            {Ground3, ShiftIndex2} = simulate1(Shape, ShiftArray, ShiftIndex, Ground2),
            %% Remove empty bits from the top of the ground.
            Ground4 = lists:dropwhile(fun(Bits) -> Bits == 2#0000000 end, Ground3),
            GroundLength = length(Ground4),
            %% Save already seen state for cycle detection.
            true = ets:insert(cycles, {Key, {N, GroundLength}}),
            %% Simulate next shape.
            simulate(
                ShapeArray, ShapeIndex2, ShiftArray, ShiftIndex2, Ground4, Height, N - 1
            )
    end.
