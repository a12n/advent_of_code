-module(day_14).

-include_lib("eunit/include/eunit.hrl").

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
                        {binary_to_integer(X), binary_to_integer(Y)}
                    end,
                    binary:split(Line, <<" -> ">>, [global])
                )
            end,
            io_ext:read_lines(standard_io)
        ),
    Rocks = lists_ext:reduce(fun sets:union/2, lists:map(fun path_to_set/1, Paths)),
    ?debugFmt("Paths ~p", [Paths]),
    ?debugFmt("Rocks ~p", [Rocks]),
    ok.

-spec path_to_set(path()) -> point_set().
path_to_set(Path) -> path_to_set(Path, sets:new([{version, 2}])).

-spec path_to_set(path(), point_set()) -> point_set().
path_to_set([], Set) ->
    Set;
path_to_set([P], Set) ->
    sets:add_element(P, Set);
path_to_set([P | PointsLeft = [Q | _]], Set) ->
    NewPoints =
        case {P, Q} of
            {{X1, Y}, {X2, Y}} ->
                %% Horizontal line.
                [{X, Y} || X <- lists_ext:seq(X1, X2)];
            {{X, Y1}, {X, Y2}} ->
                %% Vertical line.
                [{X, Y} || Y <- lists_ext:seq(Y1, Y2)]
        end,
    ?debugFmt("P ~p → Q ~p = ~p", [P, Q, NewPoints]),
    UpdatedSet = sets:union(
        Set,
        sets:from_list(
            NewPoints,
            [{version, 2}]
        )
    ),
    path_to_set(PointsLeft, UpdatedSet).
