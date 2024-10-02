-module(day_22).

-export([main/1, parse_instructions/1]).

-spec main(1..2) -> ok.
main(1) ->
    ok.

-spec parse_instructions(binary()) -> [pos_integer() | left | right].
parse_instructions(Line) ->
    (fun
        Parse(<<>>, 0) -> [];
        Parse(<<>>, N) -> [N];
        Parse(<<"L", Rest/bytes>>, 0) -> [left | Parse(Rest, 0)];
        Parse(<<"L", Rest/bytes>>, N) -> [N, left | Parse(Rest, 0)];
        Parse(<<"R", Rest/bytes>>, 0) -> [right | Parse(Rest, 0)];
        Parse(<<"R", Rest/bytes>>, N) -> [N, right | Parse(Rest, 0)];
        Parse(<<C, Rest/bytes>>, N) when C >= $0, C =< $9 -> Parse(Rest, N * 10 + (C - $0))
    end)(
        Line, 0
    ).
