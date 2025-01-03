%% Rope Bridge
-module(day_09).

-type move() :: {grids:dir(), pos_integer()}.
-type pos() :: grids:pos(integer()).

-export([main/1]).

-spec main(1..2) -> ok.
main(Part) ->
    Motions = lists:map(
        fun(<<Dir, " ", Num/bytes>>) -> {grids:char_to_dir(Dir), binary_to_integer(Num)} end,
        io_ext:read_lines(standard_io)
    ),
    NumKnots =
        case Part of
            1 -> 2;
            2 -> 10
        end,
    Visited = sets:from_list(simulate(Motions, NumKnots), [{version, 2}]),
    io:format("~b~n", [sets:size(Visited)]).

-spec simulate([move()], pos_integer()) -> [pos()].
simulate(Motions, NumKnots) ->
    simulate(Motions, lists:duplicate(NumKnots, {0, 0}), [{0, 0}]).

-spec simulate([move()], [pos()], [pos()]) -> [pos()].
simulate([], _, Visited) ->
    Visited;
simulate([{_, 0} | Motions], Knots, Visited) ->
    simulate(Motions, Knots, Visited);
simulate([{Dir, N} | Motions], [HeadKnot | Knots], Visited) ->
    HeadKnot2 = grids:add_pos(HeadKnot, grids:dir_to_pos(Dir)),
    {Knots2, LastKnot} = lists:mapfoldl(
        fun(Knot, Leading) ->
            Vector = tail_vector(grids:sub_pos(Leading, Knot)),
            Knot2 = grids:add_pos(Knot, Vector),
            {Knot2, Knot2}
        end,
        HeadKnot2,
        Knots
    ),
    simulate([{Dir, N - 1} | Motions], [HeadKnot2 | Knots2], [LastKnot | Visited]).

-spec tail_vector(pos()) -> pos().
tail_vector({R, C}) when abs(R) < 2, abs(C) < 2 -> {0, 0};
%% tail_vector({0, C}) -> {0, C div 2};
%% tail_vector({R, 0}) -> {R div 2, 0}.
tail_vector({R, C}) when R < 0, C < 0 -> {(R - 1) div 2, (C - 1) div 2};
tail_vector({R, C}) when R < 0 -> {(R - 1) div 2, (C + 1) div 2};
tail_vector({R, C}) when C < 0 -> {(R + 1) div 2, (C - 1) div 2};
tail_vector({R, C}) -> {(R + 1) div 2, (C + 1) div 2}.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

tail_vector_test() ->
    lists:foreach(
        fun(U) ->
            V = tail_vector(U),
            ?debugFmt("tail_vector ~p → ~p", [U, V])
        end,
        [{R, C} || R <- [-2, -1, 0, 1, 2], C <- [-2, -1, 0, 1, 2]]
    ).

-endif.
