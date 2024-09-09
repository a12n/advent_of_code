-module(day_09).

-type move() :: {grids:dir(), pos_integer()}.
-type pos() :: grids:pos(integer()).
-type pos_set() :: sets:set(pos()).

-export([main/1]).

-spec main(1..2) -> ok.
main(1) ->
    Motions = lists:map(
        fun(<<Dir, " ", Num/bytes>>) -> {grids:char_to_dir(Dir), binary_to_integer(Num)} end,
        io_ext:read_lines(standard_io)
    ),
    io:format("~b~n", [sets:size(simulate(Motions))]).

-spec simulate([move()]) -> pos_set().
simulate(Motions) ->
    simulate(Motions, {0, 0}, {0, 0}, sets:new([])).

-spec simulate([move()], pos(), pos(), pos_set()) -> pos_set().
simulate(Motions, Head, Tail, Visited) ->
    Visited.
