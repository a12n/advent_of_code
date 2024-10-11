%% Full of Hot Air
-module(day_25).

-type snafu() :: nonempty_list(snafu_digit()).
-type snafu_digit() :: $= | $- | $0 | $1 | $2.
-type snafu_value() :: -2..2.

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
    %% TODO: Addition in SNAFU?
    io:format("~s~n", [integer_to_snafu(lists:sum(lists:map(fun snafu_to_integer/1, Numbers)))]).

-spec snafu_to_integer(binary() | string()) -> non_neg_integer().
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

-spec integer_to_snafu(non_neg_integer()) -> string().
integer_to_snafu(N) ->
    %% TODO
    integer_to_list(N).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

snafu_to_integer_test() ->
    ?assertEqual(1, snafu_to_integer("1")),
    ?assertEqual(2, snafu_to_integer("2")),
    ?assertEqual(3, snafu_to_integer("1=")),
    ?assertEqual(4, snafu_to_integer("1-")),
    ?assertEqual(5, snafu_to_integer("10")),
    ?assertEqual(6, snafu_to_integer("11")),
    ?assertEqual(7, snafu_to_integer("12")),
    ?assertEqual(8, snafu_to_integer("2=")),
    ?assertEqual(9, snafu_to_integer("2-")),
    ?assertEqual(10, snafu_to_integer("20")),
    ?assertEqual(15, snafu_to_integer("1=0")),
    ?assertEqual(20, snafu_to_integer("1-0")),
    ?assertEqual(2022, snafu_to_integer("1=11-2")),
    ?assertEqual(12345, snafu_to_integer("1-0---0")),
    ?assertEqual(314159265, snafu_to_integer("1121-1110-1=0")).

-endif.
