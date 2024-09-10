-module(day_14).

-include_lib("eunit/include/eunit.hrl").

-type line() :: {_Begin :: pos(), _End :: pos()}.
-type path() :: [pos()].
-type pos() :: grids:pos(integer()).

%% Mapping of X coordinates to the corresponding top Y coordinates.
-type ground_level() :: #{integer() := integer()}.

%% Mapping of coordinates to rock types.
-type rocks() :: #{pos() := char()}.

-export([main/1]).

-spec main(1..2) -> ok.
main(1) ->
    Paths =
        lists:map(
            fun(<<Line/bytes>>) ->
                lists:map(
                    fun(<<Point/bytes>>) ->
                        [X, Y] = binary:split(Point, <<",">>),
                        {binary_to_integer(Y), binary_to_integer(X)}
                    end,
                    binary:split(Line, <<" -> ">>, [global])
                )
            end,
            io_ext:read_lines(standard_io)
        ),
    Source = {0, 500},
    Rocks = paths_to_rocks(Paths),
    {NumSimulations, _, _} = simulate(Rocks, ground_level(Rocks), Source),
    io:format(<<"~b~n">>, [NumSimulations]).

-spec path_to_lines(path()) -> [line()].
path_to_lines([]) -> [];
path_to_lines([P0, P1]) -> [{P0, P1}];
path_to_lines([P0 | PointsLeft = [P1 | _]]) -> [{P0, P1} | path_to_lines(PointsLeft)].

-spec line_to_positions(line()) -> [pos()].
line_to_positions({{X1, Y}, {X2, Y}}) ->
    %% Horizontal line.
    [{X, Y} || X <- lists_ext:seq(X1, X2)];
line_to_positions({{X, Y1}, {X, Y2}}) ->
    %% Vertical line.
    [{X, Y} || Y <- lists_ext:seq(Y1, Y2)].

-spec paths_to_rocks([path()]) -> rocks().
paths_to_rocks(Paths) ->
    lists:foldl(
        fun(Path, Result) ->
            lists:foldl(
                fun(Line, Result) ->
                    lists:foldl(
                        fun(Pos, Result) ->
                            Result#{Pos => $#}
                        end,
                        Result,
                        line_to_positions(Line)
                    )
                end,
                Result,
                path_to_lines(Path)
            )
        end,
        _Result = #{},
        Paths
    ).

-spec simulate(rocks(), ground_level(), pos()) -> {non_neg_integer(), rocks(), ground_level()}.
simulate(Rocks, GroundLevel, Source) ->
    {{_, MinCol}, MaxPos} = grids:extent(Rocks),
    (fun Loop(Rocks, GroundLevel, N) ->
        ?debugFmt("~nSand #~b =~n~s", [
            N,
            grids:to_iodata(Rocks#{Source => $+}, {0, MinCol}, MaxPos)
        ]),
        case simulate1(Rocks, GroundLevel, Source) of
            {true, Rocks2, GroundLevel2} ->
                Loop(Rocks2, GroundLevel2, N + 1);
            false ->
                {N, Rocks, GroundLevel}
        end
    end)(
        Rocks, GroundLevel, 0
    ).

-spec simulate1(rocks(), ground_level(), pos()) -> {true, rocks(), ground_level()} | false.
simulate1(Rocks, GroundLevel, _Sand = {Y, X}) ->
    case maps:find(X, GroundLevel) of
        {ok, GroundY} when Y < GroundY ->
            StopY = GroundY - 1,
            DownLeft = {StopY + 1, X - 1},
            DownRight = {StopY + 1, X + 1},
            case Rocks of
                #{DownLeft := _, DownRight := _} ->
                    %% Both down-left and down-right positions are
                    %% already "rock", sand comes to rest.
                    {true, Rocks#{{StopY, X} => $o}, GroundLevel#{X := StopY}};
                #{DownLeft := _} ->
                    %% Down-left position is rock, flow down-right.
                    simulate1(Rocks, GroundLevel, DownRight);
                #{} ->
                    %% Down-left position isn't set as rock, flow down-left.
                    simulate1(Rocks, GroundLevel, DownLeft)
            end;
        error ->
            %% No ground below SandPos, falls to abyss.
            false
    end.

-spec ground_level(rocks()) -> ground_level().
ground_level(Rocks) ->
    maps:fold(
        fun({Y, X}, _, Result) ->
            maps:update_with(X, fun(TopY) -> min(TopY, Y) end, Y, Result)
        end,
        #{},
        Rocks
    ).
