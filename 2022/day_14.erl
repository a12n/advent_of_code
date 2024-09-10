-module(day_14).

-include_lib("eunit/include/eunit.hrl").

-type line() :: {_Begin :: pos(), _End :: pos()}.
-type path() :: [pos()].
-type pos() :: grids:pos(integer()).

%% Mapping of X coordinates to the corresponding bottom Y
%% coordinates. If sand X isn't in this set (or it's Y is below the
%% corresponding Y in the ground level), it falls to abyss.
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
    {NumSimulations, _} = simulate(Rocks, ground_level(Rocks), Source),
    io:format(<<"~b~n">>, [NumSimulations]).

-spec path_to_lines(path()) -> [line()].
path_to_lines([]) -> [];
path_to_lines([P0, P1]) -> [{P0, P1}];
path_to_lines([P0 | PointsLeft = [P1 | _]]) -> [{P0, P1} | path_to_lines(PointsLeft)].

-spec line_to_positions(line()) -> [pos()].
line_to_positions({{Y, X1}, {Y, X2}}) ->
    %% Horizontal line.
    [{Y, X} || X <- lists_ext:seq(X1, X2)];
line_to_positions({{Y1, X}, {Y2, X}}) ->
    %% Vertical line.
    [{Y, X} || Y <- lists_ext:seq(Y1, Y2)].

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

-spec simulate(rocks(), ground_level(), pos()) -> {non_neg_integer(), rocks()}.
simulate(Rocks, GroundLevel, Source) ->
    {{_, MinCol}, MaxPos} = grids:extent(Rocks),
    (fun Loop(Rocks, N) ->
        ?debugFmt("~nSand #~b =~n~s", [
            N,
            grids:to_iodata(Rocks#{Source => $+}, {0, MinCol}, MaxPos)
        ]),
        case simulate1(Rocks, GroundLevel, Source) of
            {true, Rocks2} -> Loop(Rocks2, N + 1);
            false -> {N, Rocks}
        end
    end)(
        Rocks, 0
    ).

-spec simulate1(rocks(), ground_level(), pos()) -> {true, rocks()} | false.
simulate1(Rocks, GroundLevel, Sand = {Y, X}) ->
    ?debugFmt("Sand ~p ~p", [Y, X]),
    case maps:find(X, GroundLevel) of
        {ok, GroundY} when GroundY < Y ->
            %% Sand is below any ground, falls to abyss.
            ?debugFmt("GroundY at ~p = ~p", [X, GroundY]),
            false;
        {ok, _} ->
            Down = {Y + 1, X},
            DownLeft = {Y + 1, X - 1},
            DownRight = {Y + 1, X + 1},
            ?debugFmt("Down ~p, DownLeft ~p, DownRight ~p", [Down, DownLeft, DownRight]),
            case Rocks of
                #{Down := _, DownLeft := _, DownRight := _} ->
                    %% There are rocks down, down-left and
                    %% down-right. Nowhere to fall further, sand comes
                    %% to rest.
                    {true, Rocks#{Sand => $o}};
                #{Down := _, DownLeft := _} ->
                    %% There are rocks down and down-left, but no rock down-rock.
                    simulate1(Rocks, GroundLevel, DownRight);
                #{Down := _} ->
                    %% There's rock down, but no rocks down-left and down-right.
                    simulate1(Rocks, GroundLevel, DownLeft);
                #{} ->
                    %% There's no rocks down, down-left and down-right.
                    simulate1(Rocks, GroundLevel, Down)
            end;
        error ->
            %% No ground below sand, falls to abyss.
            ?debugFmt("No ground for sand X ~p", [X]),
            false
    end.

-spec ground_level(rocks()) -> ground_level().
ground_level(Rocks) ->
    maps:fold(
        fun({Y, X}, _, Result) ->
            maps:update_with(X, fun(BottomY) -> max(BottomY, Y) end, Y, Result)
        end,
        #{},
        Rocks
    ).
