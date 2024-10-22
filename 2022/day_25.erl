%% Full of Hot Air
-module(day_25).

-type snafu() :: nonempty_list(snafu_digit()).
-type snafu_digit() :: $= | $- | $0 | $1 | $2.
-type snafu_value() :: -2..2.

-export([main/1]).

%% "1=" + "1-" = "12" (3 + 4 = 7)
%% 5^1 | 5^0
%% ----+----
%% 1   | =
%% 1   | -
%% ---------
%% 2   | -3
%% 2 - | 2
%% 1   | 2
%% "12"

%% "11" + "2=" = "1=-" (6 + 8 = 14)
%% 5^2 | 5^1 | 5^0
%% ----+-----+----
%%     | 1   | 1
%%     | 2   | =
%% ---------------
%%     | 3   | -
%%   1 | =   | -
%% "1=-"

%% "1=11-2" + "1-0---0" = "10=00=2" (2022 + 12345 = 14367)
%% 5^7 | 5^6 | 5^5 | 5^4 | 5^3 | 5^2 | 5^1 | 5^0
%% ----+-----+-----+-----+-----+-----+-----+----
%%     | 0   | 1   | =   | 1   | 1   | -   | 2
%%     | 1   | -   | 0   | -   | -   | -   | 0
%% ---------------------------------------------
%%     | 1   | 0   | =   | 0   | 0   | =   | 2
%% "10=00=2"

%% "1=-0-2" + "12111" = "1-111=" (1747 + 906 = 2653)
%% 5^7 | 5^6 | 5^5 | 5^4 | 5^3 | 5^2 | 5^1 | 5^0
%% ----+-----+-----+-----+-----+-----+-----+------
%%     |     |   1 |   = |   - |   0 |   - |   2
%%     |     |   0 |   1 |   2 |   1 |   1 |   1
%% -----------------------------------------------
%%     |     |   1 |   - |   1 |   1 |   0 |   3
%%     |     |   1 |   - |   1 |   1 | 0 1 | 3 =
%%     |     |   1 |   - |   1 |   1 |   1 |   =
%% "1-111="

-spec main(1) -> ok.
main(1) ->
    Numbers = lists:map(fun binary_to_list/1, io_ext:read_lines(standard_io)),
    io:format("~s~n", [integer_to_snafu(lists:sum(lists:map(fun snafu_to_integer/1, Numbers)))]).

-spec snafu_to_integer(snafu()) -> non_neg_integer().
snafu_to_integer(N = [_ | _]) ->
    {Ans, _} = lists:foldr(
        fun(Digit, {Ans, Pow5}) ->
            {Ans + digit_to_value(Digit) * Pow5, 5 * Pow5}
        end,
        {0, 1},
        N
    ),
    Ans.

-spec digit_to_value(snafu_digit()) -> snafu_value().
digit_to_value($2) -> 2;
digit_to_value($1) -> 1;
digit_to_value($0) -> 0;
digit_to_value($-) -> -1;
digit_to_value($=) -> -2.

-spec integer_to_snafu(non_neg_integer()) -> snafu().
integer_to_snafu(0) -> "0";
integer_to_snafu(N) when N > 0 -> integer_to_snafu(N, "").

-spec integer_to_snafu(non_neg_integer(), string()) -> snafu().
integer_to_snafu(0, Ans) ->
    Ans;
integer_to_snafu(N, Ans) ->
    case N rem 5 of
        M when M < 3 -> integer_to_snafu(N div 5, [M + $0 | Ans]);
        3 -> integer_to_snafu(N div 5 + 1, [$= | Ans]);
        4 -> integer_to_snafu(N div 5 + 1, [$- | Ans])
    end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

test_numbers() ->
    [
        {1, "1"},
        {2, "2"},
        {3, "1="},
        {4, "1-"},
        {5, "10"},
        {6, "11"},
        {7, "12"},
        {8, "2="},
        {9, "2-"},
        {10, "20"},
        {15, "1=0"},
        {20, "1-0"},
        {2022, "1=11-2"},
        {12345, "1-0---0"},
        {314159265, "1121-1110-1=0"}
    ].

snafu_to_integer_test_() ->
    lists:map(fun({N, M}) -> ?_assertEqual(N, snafu_to_integer(M)) end, test_numbers()).

integer_to_snafu_test_() ->
    lists:map(fun({N, M}) -> ?_assertEqual(M, integer_to_snafu(N)) end, test_numbers()).

-endif.
