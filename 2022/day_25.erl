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

-spec main(1..2) -> ok.
main(1) ->
    Numbers = lists:map(fun binary_to_list/1, io_ext:read_lines(standard_io)),
    lists:foreach(
        fun(SNAFU) ->
            io:format(standard_error, "~s	~b~n", [SNAFU, snafu_to_integer(SNAFU)])
        end,
        Numbers
    ),
    Sum = lists:foldl(fun add_snafu/2, "0", Numbers),
    io:format(standard_error, "~s	~b~n", [Sum, snafu_to_integer(Sum)]),
    io:format("~s~n", [Sum]).

-spec digit_to_value(snafu_digit()) -> snafu_value().
digit_to_value($2) -> 2;
digit_to_value($1) -> 1;
digit_to_value($0) -> 0;
digit_to_value($-) -> -1;
digit_to_value($=) -> -2.

-spec value_to_digit(snafu_value()) -> snafu_digit().
value_to_digit(-2) -> $=;
value_to_digit(-1) -> $-;
value_to_digit(0) -> $0;
value_to_digit(1) -> $1;
value_to_digit(2) -> $2.

-spec digits_to_values(snafu()) -> nonempty_list(snafu_value()).
digits_to_values(Digits) -> lists:map(fun digit_to_value/1, Digits).

-spec values_to_digits(nonempty_list(snafu_value())) -> snafu().
values_to_digits(Values) -> lists:map(fun value_to_digit/1, Values).

-spec add_snafu(snafu(), snafu()) -> snafu().
add_snafu(N = [_ | _], M = [_ | _]) ->
    %% Compute the necessary padding to align N and M.
    {PadN, PadM} =
        case {length(N), length(M)} of
            {LenN, LenM} when LenN > LenM -> {"", lists:duplicate(LenN - LenM, $0)};
            {LenN, LenM} when LenN < LenM -> {lists:duplicate(LenM - LenN, $0), ""};
            {Len, Len} -> {"", ""}
        end,
    %% Was adding starting from lower positions, reverse the resulting
    %% list of values. Encode values as digits.
    values_to_digits(
        lists:reverse(
            %% Add digits for each position, starting from the
            %% lowest. Carry overflows/underflows.
            add_snafu_rev_values(
                %% Pad and align lists of digit values.
                digits_to_values(lists:reverse(N, PadN)),
                digits_to_values(lists:reverse(M, PadM)),
                0
            )
        )
    ).

-spec add_snafu_rev_values([-2..2], [-2..2], -1..1) -> [-2..2].
add_snafu_rev_values([], [], 0) ->
    [];
add_snafu_rev_values([], [], Carry) ->
    [Carry];
add_snafu_rev_values([Digit1 | Values1], [Digit2 | Values2], Carry) ->
    case Digit1 + Digit2 + Carry of
        Ans when Ans > 2 -> [(Ans - 5) | add_snafu_rev_values(Values1, Values2, 1)];
        Ans when Ans < -2 -> [(Ans + 5) | add_snafu_rev_values(Values1, Values2, -1)];
        Ans -> [Ans | add_snafu_rev_values(Values1, Values2, 0)]
    end.

-spec snafu_to_integer(snafu()) -> non_neg_integer().
snafu_to_integer(N) ->
    {Values, _} = lists:mapfoldr(
        fun(Value, Pow5) -> {Value * Pow5, 5 * Pow5} end, 1, digits_to_values(N)
    ),
    lists:sum(Values).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

add_snafu_test() ->
    %% 3 + 4 = 7
    ?assertEqual("12", add_snafu("1=", "1-")),
    %% 6 + 8 = 14
    ?assertEqual("1=-", add_snafu("11", "2=")),
    %% 2022 + 12345 = 14367
    ?assertEqual("10=00=2", add_snafu("1=11-2", "1-0---0")),
    %% 1747 + 906 = 2653
    ?assertEqual("1-111=", add_snafu("1=-0-2", "12111")).

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
