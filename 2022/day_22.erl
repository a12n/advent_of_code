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
            2 -> cube_wrapping(Grid)
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

%% XXX: Manually crafted to work only for a few specific nets of a cube.
%%
%% To find out the gluing make the following net (from a piece of
%% paper) of a cube and try to fold it. This way it's obvious which
%% side glues to which side and whether coordinates of a side must be
%% reversed.
%%
%%                             ░░░→░░U░░░░░░
%%                             ↓░░░░░░░░░░░↓
%%                             ░░░░░░░░░░░░░
%%                             L░░░░1,3░░░░R
%%                             ░░░░░░░░░░░░░
%%                             ░░░░░░░░░░░░░
%%                             ░░░░░░░░░░░░░
%%                             -------------
%% ░░░→░░U░░░░░░|░░░→░░U░░░░░░|░░░░░░░░░░░░░
%% ↓░░░░░░░░░░░░|░░░░░░░░░░░░░|░░░░░░░░░░░░↓
%% ░░░░░░░░░░░░░|░░░░░░░░░░░░░|░░░░░░░░░░░░░
%% L░░░░2,1░░░░░|░░░░░2,2░░░░░|░░░░░2,3░░░░R
%% ░░░░░░░░░░░░░|░░░░░░░░░░░░░|░░░░░░░░░░░░░
%% ░░░░░░░░░░░░░|░░░░░░░░░░░░░|░░░░░░░░░░░░░
%% ░░░→░░D░░░░░░|░░░→░░D░░░░░░|░░░░░░░░░░░░░
%%                             -------------
%%                             ░░░░░░░░░░░░░|░░░→░░U░░░░░░
%%                             ↓░░░░░░░░░░░░|░░░░░░░░░░░░↓
%%                             ░░░░░░░░░░░░░|░░░░░░░░░░░░░
%%                             L░░░░3,3░░░░░|░░░░░3,4░░░░R
%%                             ░░░░░░░░░░░░░|░░░░░░░░░░░░░
%%                             ░░░░░░░░░░░░░|░░░░░░░░░░░░░
%%                             ░░░→░░D░░░░░░|░░░→░░D░░░░░░
-spec cube_wrapping(grids:grid(?OPEN | ?WALL)) -> wrapping().
cube_wrapping(Grid) ->
    {FaceSize, FaceGluing} =
        case maps:size(Grid) div 6 of
            4 * 4 ->
                %% Sample input, configuration:
                %% ... ... 1,3 ...
                %% 2,1 2,2 2,3 ...
                %% ... ... 3,3 3,4
                {4, #{
                    {1, 3} => #{
                        up => {{2, 1}, up, reverse},
                        left => {{2, 2}, up, forward},
                        right => {{3, 4}, right, reverse}
                    },
                    {2, 1} => #{
                        up => {{1, 3}, up, reverse},
                        left => {{3, 4}, down, reverse},
                        down => {{3, 3}, down, reverse}
                    },
                    {2, 2} => #{
                        up => {{1, 3}, left, forward},
                        down => {{3, 3}, left, reverse}
                    },
                    {2, 3} => #{
                        right => {{3, 4}, up, reverse}
                    },
                    {3, 3} => #{
                        left => {{2, 2}, down, reverse},
                        down => {{2, 1}, down, reverse}
                    },
                    {3, 4} => #{
                        up => {{2, 3}, right, reverse},
                        right => {{1, 3}, right, reverse},
                        down => {{2, 1}, left, reverse}
                    }
                }};
            50 * 50 ->
                %% Puzzle input, configuration:
                %% ... 1,2 1,3
                %% ... 2,2 ...
                %% 3,1 3,2 ...
                %% 4,1 ... ...
                %% TODO
                {50, #{
                    {1, 2} => #{
                        up => {{4, 1}, left, forward},
                        left => {{3, 1}, left, reverse}
                    },
                    {1, 3} => #{
                        up => {{4, 1}, down, forward},
                        right => {{3, 2}, right, reverse},
                        down => {{2, 2}, right, forward}
                    },
                    {2, 2} => #{
                        left => {{3, 1}, up, forward},
                        right => {{1, 3}, down, forward}
                    },
                    {3, 1} => #{
                        up => {{2, 2}, left, forward},
                        left => {{1, 2}, left, reverse}
                    },
                    {3, 2} => #{
                        right => {{1, 3}, right, reverse},
                        down => {{4, 1}, right, forward}
                    },
                    {4, 1} => #{
                        left => {{1, 2}, up, forward},
                        right => {{3, 2}, down, forward},
                        down => {{1, 3}, up, forward}
                    }
                }}
        end,
    filter_wrapping(
        Grid,
        maps:fold(
            fun(FromFace, Gluing, Wrapping) ->
                maps:fold(
                    fun(FromSide, {ToFace, ToSide, Order}, Wrapping2) ->
                        FromPos = grids:extent_side(face_extent(FaceSize, FromFace), FromSide),
                        ToPos = grids:extent_side(face_extent(FaceSize, ToFace), ToSide),
                        FromPos2 =
                            lists:map(
                                fun(Pos) ->
                                    grids:add_pos(Pos, grids:dir_to_pos(FromSide))
                                end,
                                FromPos
                            ),
                        ToPos2 =
                            case Order of
                                reverse -> lists:reverse(ToPos);
                                forward -> ToPos
                            end,
                        lists:foldl(
                            fun({FromP, ToP}, Wrapping3) ->
                                maps:put({FromP, FromSide}, {ToP, grids:neg_dir(ToSide)}, Wrapping3)
                            end,
                            Wrapping2,
                            lists:zip(FromPos2, ToPos2)
                        )
                    end,
                    Wrapping,
                    Gluing
                )
            end,
            #{},
            FaceGluing
        )
    ).

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
