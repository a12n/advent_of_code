%% Monkey Map
-module(day_22).

-export([main/1, parse_instructions/1]).

-type instruction() :: pos_integer() | ccw | cw.
-type move_map() :: #{grids:pos() => #{grids:dir() => grids:pos()}}.

-define(OPEN, $.).
-define(WALL, $#).

-spec main(1..2) -> ok.
main(1) ->
    {Grid, Extent, Start, Instructions} = parse_input(io_ext:read_lines(standard_io)),
    io:format(standard_error, "Start ~p, Extent ~p~n", [Start, Extent]),
    io:format(standard_error, "Instructions ~p~n", [Instructions]),
    {Finish, FinishDir} = simulate(grid_to_move_map(Grid), Start, right, Instructions),
    io:format(standard_error, "Finish ~p, FinishDir ~p~n", [Finish, FinishDir]),
    io:format("~b~n", [password(Finish, FinishDir)]).

-spec password(grids:pos(), grids:dir()) -> non_neg_integer().
password({Row, Col}, Dir) ->
    1000 * Row + 4 * Col +
        case Dir of
            right -> 0;
            down -> 1;
            left -> 2;
            up -> 3
        end.

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

-spec simulate(move_map(), grids:pos(), grids:dir(), [instruction()]) -> {grids:pos(), grids:dir()}.
simulate(MoveMap, Pos0, Dir0, Instructions) ->
    (fun
        Loop(Pos, Dir, []) ->
            %% No more instructions, return position and direction.
            {Pos, Dir};
        Loop(Pos, Dir, [Rotate | NextInstructions]) when Rotate == ccw; Rotate == cw ->
            %% Rotate direction.
            Loop(Pos, grids:rotate_dir(Rotate, Dir), NextInstructions);
        Loop(Pos, Dir, [0 | NextInstructions]) ->
            %% Moved in a direction, next instruction.
            Loop(Pos, Dir, NextInstructions);
        Loop(Pos, Dir, [N | NextInstructions]) when N > 0 ->
            %% Try moving to a direction.
            case MoveMap of
                #{Pos := #{Dir := Pos2}} ->
                    %% There's a next position, do move.
                    Loop(Pos2, Dir, [(N - 1) | NextInstructions]);
                _NoMove ->
                    %% No next position from here, go to the next instruction.
                    Loop(Pos, Dir, NextInstructions)
            end
    end)(
        Pos0, Dir0, Instructions
    ).

-spec grid_to_move_map(grids:grid()) -> move_map().
grid_to_move_map(Grid) ->
    {{MinRow, MinCol}, {MaxRow, MaxCol}} = grids:extent(Grid),
    FirstNonBlank = fun Loop(Pos, Off) ->
        case maps:find(Pos, Grid) of
            {ok, $.} -> Pos;
            {ok, $#} -> undefined;
            error -> Loop(grids:add_pos(Pos, Off), Off)
        end
    end,
    Next = fun(Dir, Pos = {Row, Col}) ->
        Off = grids:dir_to_pos(Dir),
        Pos2 = grids:add_pos(Pos, Off),
        case maps:find(Pos2, Grid) of
            {ok, $.} -> Pos2;
            {ok, $#} -> undefined;
            error when Dir == left -> FirstNonBlank({Row, MaxCol}, Off);
            error when Dir == right -> FirstNonBlank({Row, MinCol}, Off);
            error when Dir == up -> FirstNonBlank({MaxRow, Col}, Off);
            error when Dir == down -> FirstNonBlank({MinRow, Col}, Off)
        end
    end,
    maps:filtermap(
        fun
            (Pos, $.) ->
                {true,
                    maps:filter(
                        fun
                            (_, {_, _}) -> true;
                            (_, undefined) -> false
                        end,
                        #{
                            up => Next(up, Pos),
                            left => Next(left, Pos),
                            right => Next(right, Pos),
                            down => Next(down, Pos)
                        }
                    )};
            (_, $#) ->
                false
        end,
        Grid
    ).
