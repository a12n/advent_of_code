-module(day_06).

-export([main/1]).

-spec main(1..2) -> ok.
main(1) ->
    ok = io:setopts(standard_io, [list]),
    [Signal] = io_ext:read_lines(standard_io),
    io:format(<<"~b~n">>, [find_marker(Signal)]).

-spec find_marker(string()) -> non_neg_integer().
find_marker(Signal) -> find_marker(Signal, 4).

-spec find_marker(string(), non_neg_integer()) -> non_neg_integer().
find_marker([A, B, C, D | _], N) when
    not (A == B orelse A == C orelse A == D orelse
        B == C orelse B == D orelse
        C == D)
->
    N;
find_marker([_ | Signal], N) ->
    find_marker(Signal, N + 1).
