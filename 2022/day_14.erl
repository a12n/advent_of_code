-module(day_14).

-include_lib("eunit/include/eunit.hrl").

-type line() :: {point(), point()}.
-type path() :: [point()].
-type point() :: {integer(), integer()}.
-type point_set() :: sets:set(point()).

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
    Rocks = maps:map(
        fun(_, _) ->
            %% Make rocks printable with grids:to_iodata/3.
            $#
        end,
        paths_to_point_set(Paths)
    ),
    ?debugFmt("Paths ~p", [Paths]),
    ?debugFmt("Rocks ~p", [Rocks]),
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
                        fun sets:add_element/2,
                        Result,
                        line_to_points(Line)
                    )
                end,
                Result,
                path_to_lines(Path)
            )
        end,
        _Result = sets:new([{version, 2}]),
        Paths
    ).
