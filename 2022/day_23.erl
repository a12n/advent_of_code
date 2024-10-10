-module(day_23).

-export([main/1]).
-export([rotate/1]).

-spec main(1..2) -> ok.
main(Part) ->
    Grid = maps:filter(
        fun
            (_, $#) -> true;
            (_, $.) -> false
        end,
        grids:from_lines(io_ext:read_lines(standard_io))
    ),
    io:format(standard_error, "Grid =~n~s", [
        grids:to_iodata(Grid, grids:extent(Grid), #{blank => $.})
    ]),
    NumRounds =
        case Part of
            1 -> 10;
            2 -> infinity
        end,
    {Grid2, Round} = disperse(Grid, NumRounds),
    Extent = grids:extent(Grid2),
    io:format(standard_error, "Grid2 =~n~s", [grids:to_iodata(Grid2, Extent, #{blank => $.})]),
    case Part of
        1 ->
            Area = grids:extent_area(Extent),
            io:format(standard_error, "Extent ~p, Area ~p~n", [Extent, Area]),
            io:format("~b~n", [Area - maps:size(Grid2)]);
        2 ->
            io:format("~b~n", [Round])
    end.

-spec disperse(grids:grid(integer()), pos_integer()) -> {grids:grid(integer()), non_neg_integer()}.
disperse(Grid0, NumRounds) ->
    NeighborDirs = [
        {Row, Col}
     || Row <- [-1, 0, 1],
        Col <- [-1, 0, 1],
        not (Row == 0 andalso Col == 0)
    ],
    (fun
        Loop(Grid, _, Round) when Round == NumRounds -> {Grid, Round};
        Loop(Grid, Dirs, Round) ->
            Proposed = maps:fold(
                fun(Pos, _, Proposed) ->
                    case has_neighbor(Grid, Pos, NeighborDirs) of
                        true ->
                            %% Some neighbour in one of eight directions, have to move.
                            case
                                %% Find first non-occupied adjacent positions in the propsed direction.
                                lists:dropwhile(
                                    fun(Dir) ->
                                        has_neighbor(Grid, Pos, adjacent(Dir))
                                    end,
                                    Dirs
                                )
                            of
                                [] ->
                                    %% All adjacent positions are occupied, can't move.
                                    Proposed;
                                [Dir | _] ->
                                    %% Can move to a adjacent direction, update propesed moves map.
                                    ProposedPos = grids:add_pos(Pos, Dir),
                                    maps:update_with(
                                        ProposedPos,
                                        fun(Elves) -> [Pos | Elves] end,
                                        [Pos],
                                        Proposed
                                    )
                            end;
                        false ->
                            %% No neighbours, don't move.
                            Proposed
                    end
                end,
                #{},
                Grid
            ),
            case maps:size(Proposed) of
                0 ->
                    %% No proposed moves, elves dispersed, return the resulting grid.
                    {Grid, Round};
                _ ->
                    %% Apply the proposed moves.
                    Grid2 = maps:fold(
                        fun
                            (NewPos, [OldPos], ResultGrid) ->
                                %% Move single elf to a new position.
                                {Elf, ResultGrid2} = maps:take(OldPos, ResultGrid),
                                maps:put(NewPos, Elf, ResultGrid2);
                            (_, [_ | _], ResultGrid) ->
                                %% More than one elf proposed the position, don't move any of them.
                                ResultGrid
                        end,
                        Grid,
                        Proposed
                    ),
                    Loop(Grid2, rotate(Dirs), Round + 1)
            end
    end)(
        Grid0, lists:map(fun grids:dir_to_pos/1, [up, down, left, right]), 0
    ).

-spec has_neighbor(grids:grid(integer()), grids:pos(integer()), [grids:pos(integer())]) ->
    boolean().
has_neighbor(Grid, Pos, Dirs) ->
    lists:any(
        fun(Dir) ->
            maps:is_key(grids:add_pos(Pos, Dir), Grid)
        end,
        Dirs
    ).

-spec adjacent(grids:pos(-1..1)) -> [grids:pos(-1..1)].
adjacent({-1, 0}) -> [{-1, -1}, {-1, 0}, {-1, 1}];
adjacent({1, 0}) -> [{1, 1}, {1, 0}, {1, -1}];
adjacent({0, -1}) -> [{1, -1}, {0, -1}, {-1, -1}];
adjacent({0, 1}) -> [{-1, 1}, {0, 1}, {1, 1}].

-spec rotate(list()) -> list().
rotate([Head | Tail]) ->
    (fun
        Loop([]) -> [Head];
        Loop([Elt | List]) -> [Elt | Loop(List)]
    end)(
        Tail
    ).
