-module(day_22).

-export([main/1, parse_instructions/1]).

-type instruction() :: pos_integer() | ccw | cw.

-spec main(1..2) -> ok.
main(1) ->
    {Grid, Path} = parse(io_ext:read_lines(standard_io)),
    io:format(standard_error, "Grid ~p~n", [Grid]),
    io:format(standard_error, "Grid~n~s", [grids:to_iodata(Grid)]),
    io:format(standard_error, "Path ~p~n", [Path]),
    ok.

-spec parse([binary()]) -> {grids:grid(), [instruction()]}.
parse(Lines) ->
    {
        maps:filtermap(
            fun
                (_, $.) -> true;
                (_, $#) -> true;
                (_, $\s) -> false
            end,
            grids:from_lines(lists:droplast(Lines))
        ),
        parse_instructions(lists:last(Lines))
    }.

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
