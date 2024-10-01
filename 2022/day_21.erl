-module(day_21).

-export([main/1]).

-spec main(1..2) -> ok.
main(1) ->
    MonkeyDescrs =
        lists:map(
            fun(Line) ->
                [ID | Descr] = binary:split(Line, [<<" ">>, <<":">>], [global, trim_all]),
                case Descr of
                    [Number] ->
                        {ID, binary_to_integer(Number)};
                    [LeftID, <<Op>>, RightID] ->
                        {ID,
                            case Op of
                                $+ -> fun erlang:'+'/2;
                                $- -> fun erlang:'-'/2;
                                $* -> fun erlang:'*'/2;
                                $/ -> fun erlang:'/'/2
                            end, LeftID, RightID}
                end
            end,
            io_ext:read_lines(standard_io)
        ),
    io:format(standard_error, "MonkeyDescrs ~p~n", [MonkeyDescrs]),
    ok.
