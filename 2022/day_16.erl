-module(day_16).

-export([main/1]).

-spec main(1..2) -> ok.
main(1) ->
    Valves = maps:from_list(lists:map(fun parse_valve/1, io_ext:read_lines(standard_io))),
    io:format(standard_error, <<"~p valves ~p~n">>, [maps:size(Valves), Valves]),
    ok.

-spec parse_valve(binary()) -> {binary(), {non_neg_integer(), [binary()]}}.
parse_valve(Line) ->
    [
        <<"Valve">>,
        ID,
        <<"has">>,
        <<"flow">>,
        <<"rate">>,
        FlowStr,
        <<"tunnel", _/bytes>>,
        <<"lead", _/bytes>>,
        <<"to">>,
        <<"valve", _/bytes>>
        | LeadsTo
    ] =
        binary:split(Line, [<<" ">>, <<",">>, <<"=">>, <<";">>], [global, trim, trim_all]),
    {ID, {binary_to_integer(FlowStr), LeadsTo}}.
