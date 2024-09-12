-module(day_16).

-export([main/1]).

-spec main(1..2) -> ok.
main(1) ->
    Valves = lists:foldl(
        fun(<<Line/bytes>>, Result) ->
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
                | LeadTo
            ] =
                binary:split(Line, [<<" ">>, <<",">>, <<"=">>, <<";">>], [global, trim, trim_all]),
            Result#{ID => {binary_to_integer(FlowStr), LeadTo}}
        end,
        #{},
        io_ext:read_lines(standard_io)
    ),
    io:format(standard_error, <<"~p valves ~p~n">>, [maps:size(Valves), Valves]),
    ok.
