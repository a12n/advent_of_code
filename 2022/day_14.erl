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
    Rocks = paths_to_point_set(Paths),
    {{_, MinCol}, MaxPos} = grids:extent(Rocks),
    ?debugFmt("Paths ~p", [Paths]),
    ?debugFmt("Rocks ~p", [Rocks]),
    io:format(standard_error, <<"Rocks =~n~s">>, [
        grids:to_iodata(Rocks#{Source => $+}, {0, MinCol}, MaxPos)
    ]),
    ok.

-spec path_to_lines(path()) -> [line()].
path_to_lines([]) -> [];
path_to_lines([P0, P1]) -> [{P0, P1}];
path_to_lines([P0 | PointsLeft = [P1 | _]]) -> [{P0, P1} | path_to_lines(PointsLeft)].

-spec line_to_points(line()) -> [point()].
line_to_points({{X1, Y}, {X2, Y}}) ->
    %% Horizontal line.
    [{X, Y} || X <- lists_ext:seq(X1, X2)];
line_to_points({{X, Y1}, {X, Y2}}) ->
    %% Vertical line.
    [{X, Y} || Y <- lists_ext:seq(Y1, Y2)].

-spec paths_to_point_set([path()]) -> point_set().
paths_to_point_set(Paths) ->
    lists:foldl(
        fun(Path, Result) ->
            lists:foldl(
                fun(Line, Result) ->
                    lists:foldl(
                        fun(Point, Result) ->
                            Result#{Point => $#}
                        end,
                        Result,
                        line_to_points(Line)
                    )
                end,
                Result,
                path_to_lines(Path)
            )
        end,
        _Result = #{},
        Paths
    ).

-spec ground_level(rocks()) -> ground_level().
ground_level(Rocks) ->
    maps:fold(
        fun({Y, X}, _, Result) ->
            maps:update_with(X, fun(TopY) -> min(TopY, Y) end, Y, Result)
        end,
        #{},
        Rocks
    ).
