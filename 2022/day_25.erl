%% Full of Hot Air
-module(day_25).

-export([main/1]).

-spec main(1..2) -> ok.
main(1) ->
    Numbers = io_ext:read_lines(standard_io),
    lists:foreach(
        fun(SNAFU) ->
            io:format(standard_error, "~s	~b~n", [SNAFU, snafu_to_integer(SNAFU)])
        end,
        Numbers
    ),
    ok.

-spec snafu_to_integer(binary() | string()) -> integer().
snafu_to_integer(SNAFU) when is_binary(SNAFU) -> snafu_to_integer(binary_to_list(SNAFU));
snafu_to_integer(SNAFU) when is_list(SNAFU) ->
    {Digits, _} = lists:mapfoldl(
        fun(Digit5, Pow5) ->
            {
                Pow5 *
                    case Digit5 of
                        $2 -> 2;
                        $1 -> 1;
                        $0 -> 0;
                        $- -> -1;
                        $= -> -2
                    end,
                Pow5 * 5
            }
        end,
        1,
        lists:reverse(SNAFU)
    ),
    lists:sum(Digits).
