%% Monkey Map
-module(day_22).

-export([main/1]).

-type instruction() :: pos_integer() | ccw | cw.
-type edge() :: {grids:pos(integer()), grids:dir()}.
-type wrapping() :: #{edge() => edge()}.

-define(OPEN, $.).
-define(WALL, $#).

-spec main(1..2) -> ok.
main(Part) ->
    {Grid, Extent, Start, Instructions} = parse_input(io_ext:read_lines(standard_io)),
    io:format(standard_error, "Start ~p, Extent ~p~n", [Start, Extent]),
    io:format(standard_error, "Instructions ~p~n", [Instructions]),
    {Finish, FinishDir} = walk(
        Grid,
        case Part of
            1 -> plain_wrapping(Grid, Extent);
            2 -> cube_wrapping(Grid, Extent)
        end,
        Start,
        right,
        Instructions
    ),
    io:format(standard_error, "Finish ~p, FinishDir ~p~n", [Finish, FinishDir]),
    io:format("~b~n", [password(Finish, FinishDir)]).

%%--------------------------------------------------------------------
%% Parse input.
%%--------------------------------------------------------------------

-spec parse_input([binary()]) ->
    {grids:grid(?OPEN | ?WALL), grids:extent(), grids:pos(), nonempty_list(instruction())}.
parse_input(Lines) ->
    Grid = maps:filter(
        fun
            (_, ?OPEN) -> true;
            (_, ?WALL) -> true;
            (_, _) -> false
        end,
        grids:from_lines(lists:droplast(Lines))
    ),
    Extent = {{_, MinCol}, {_, MaxCol}} = grids:extent(Grid),
    [StartCol | _] = lists:dropwhile(
        fun(Col) -> maps:get({1, Col}, Grid, $\s) /= ?OPEN end, lists:seq(MinCol, MaxCol)
    ),
    {Grid, Extent, {1, StartCol}, parse_instructions(lists:last(Lines))}.

-spec parse_instructions(binary()) -> [instruction()].
parse_instructions(Line) ->
    (fun
        Parse(<<>>, 0) -> [];
        Parse(<<>>, N) -> [N];
        Parse(<<"L", Rest/bytes>>, 0) -> [ccw | Parse(Rest, 0)];
        Parse(<<"L", Rest/bytes>>, N) -> [N, ccw | Parse(Rest, 0)];
        Parse(<<"R", Rest/bytes>>, 0) -> [cw | Parse(Rest, 0)];
        Parse(<<"R", Rest/bytes>>, N) -> [N, cw | Parse(Rest, 0)];
        Parse(<<C, Rest/bytes>>, N) when C >= $0, C =< $9 -> Parse(Rest, N * 10 + (C - $0))
    end)(
        Line, 0
    ).

%%--------------------------------------------------------------------
%% Grid walking and password functions.
%%--------------------------------------------------------------------

-spec password(grids:pos(), grids:dir()) -> non_neg_integer().
password({Row, Col}, Dir) ->
    1000 * Row + 4 * Col +
        case Dir of
            right -> 0;
            down -> 1;
            left -> 2;
            up -> 3
        end.

-spec walk(grids:grid(?OPEN | ?WALL), wrapping(), grids:pos(), grids:dir(), [instruction()]) ->
    {grids:pos(), grids:dir()}.
walk(_, _, Pos, Dir, []) ->
    %% No more instructions, the result is current position and
    %% direction.
    {Pos, Dir};
walk(Grid, Wrapping, Pos, Dir, [Rotate | Instructions]) when Rotate == ccw; Rotate == cw ->
    %% Next instruction is rotation. Rotate the direction.
    walk(Grid, Wrapping, Pos, grids:rotate_dir(Rotate, Dir), Instructions);
walk(Grid, Wrapping, Pos, Dir, [0 | Instructions]) ->
    %% Completed the move in a direction, next instruction.
    walk(Grid, Wrapping, Pos, Dir, Instructions);
walk(Grid, Wrapping, Pos, Dir, [N | Instructions]) when N > 0 ->
    %% Moving in a direction, try taking the next step.
    NextPos = grids:add_pos(Pos, grids:dir_to_pos(Dir)),
    case Grid of
        #{NextPos := ?OPEN} ->
            %% Next position is available, do move.
            walk(Grid, Wrapping, NextPos, Dir, [(N - 1) | Instructions]);
        #{NextPos := ?WALL} ->
            %% Next position is blocked, go to the next instruction.
            walk(Grid, Wrapping, Pos, Dir, Instructions);
        _NoMove ->
            %% Went past an edge of the grid, try to wrap around to
            %% some other edge.
            case Wrapping of
                #{{NextPos, Dir} := {WrapPos, WrapDir}} ->
                    %% The wrap around position is available, do move.
                    walk(Grid, Wrapping, WrapPos, WrapDir, [(N - 1) | Instructions]);
                _NoWrap ->
                    %% The wrap around position is blocked, go to the
                    %% next instruction.
                    walk(Grid, Wrapping, Pos, Dir, Instructions)
            end
    end.

%%--------------------------------------------------------------------
%% Plain wrapping rules.
%%--------------------------------------------------------------------

-spec plain_wrapping(grids:grid(?OPEN | ?WALL), grids:extent()) -> wrapping().
plain_wrapping(Grid, Extent) -> plain_wrapping(Grid, Extent, edges(Grid)).

-spec plain_wrapping(grids:grid(?OPEN | ?WALL), grids:extent(), #{edge() => []}) -> wrapping().
plain_wrapping(Grid, {{MinRow, MinCol}, {MaxRow, MaxCol}}, Edges) ->
    filter_wrapping(
        Grid,
        maps:map(
            fun({{Row, Col}, EdgeDir}, []) ->
                WrapPos =
                    first_non_blank(
                        Grid,
                        case EdgeDir of
                            left -> {Row, MaxCol};
                            right -> {Row, MinCol};
                            up -> {MaxRow, Col};
                            down -> {MinRow, Col}
                        end,
                        grids:dir_to_pos(EdgeDir)
                    ),
                {WrapPos, EdgeDir}
            end,
            Edges
        )
    ).

-spec first_non_blank(grids:grid(), grids:pos(), grids:pos()) -> grids:pos().
first_non_blank(Grid, Pos, _) when is_map_key(Pos, Grid) -> Pos;
first_non_blank(Grid, Pos, Incr) -> first_non_blank(Grid, grids:add_pos(Pos, Incr), Incr).

%% @doc
%% Keep only wrapping positions which lead to ?OPEN positions.
%% @end
-spec filter_wrapping(grids:grid(?OPEN | ?WALL), wrapping()) -> wrapping().
filter_wrapping(Grid, Wrapping) ->
    maps:filter(
        fun({NextPos, Dir}, {WrapPos, _}) ->
            Pos = grids:add_pos(NextPos, grids:dir_to_pos(grids:neg_dir(Dir))),
            case Grid of
                #{Pos := ?OPEN, WrapPos := ?OPEN} -> true;
                _Other -> false
            end
        end,
        Wrapping
    ).

%%--------------------------------------------------------------------
%% Cube wrapping rules.
%%--------------------------------------------------------------------

-spec cube_wrapping(grids:grid(?OPEN | ?WALL), grids:extent()) -> wrapping().
cube_wrapping(Grid, Extent) -> cube_wrapping(Grid, Extent, edges(Grid)).

-spec cube_wrapping(grids:grid(?OPEN | ?WALL), grids:extent(), #{edge() => []}) -> wrapping().
cube_wrapping(Grid, {{MinRow, MinCol}, {MaxRow, MaxCol}}, Edges) ->
    %% TODO
    #{}.

%%--------------------------------------------------------------------
%% Set of grid edges (from ?OPEN positions).
%%--------------------------------------------------------------------

-spec face_extent(pos_integer(), grids:pos()) -> grids:extent().
face_extent(FaceSize, {FaceRow, FaceCol}) ->
    MinPos = {(FaceRow - 1) * FaceSize + 1, (FaceCol - 1) * FaceSize + 1},
    MaxPos = {FaceRow * FaceSize, FaceCol * FaceSize},
    {MinPos, MaxPos}.

-spec edges(grids:grid(?OPEN | ?WALL)) -> #{edge() => []}.
edges(Grid) ->
    maps:fold(
        fun
            (_, ?WALL, Edges) ->
                Edges;
            (Pos, ?OPEN, Edges) ->
                lists:foldl(
                    fun
                        ({EdgePos, _}, Edges) when is_map_key(EdgePos, Grid) ->
                            %% The adjacent position is still in the
                            %% grid. Don't update the set of edges.
                            Edges;
                        (Edge, Edges) ->
                            %% The adjacent position is outside of the
                            %% grid. Add to edges.
                            maps:put(Edge, [], Edges)
                    end,
                    Edges,
                    [
                        {grids:add_pos(Pos, grids:dir_to_pos(Dir)), Dir}
                     || Dir <- [up, left, right, down]
                    ]
                )
        end,
        #{},
        Grid
    ).
