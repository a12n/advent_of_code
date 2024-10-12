%% Tuning Trouble
-module(day_06).

-include_lib("eunit/include/eunit.hrl").

-export([main/1]).

-spec main(1..2) -> ok.
main(Part) ->
    [Signal] = io_ext:read_lines(standard_io),
    io:format(<<"~b~n">>, [
        find_marker(
            Signal,
            case Part of
                1 -> 4;
                2 -> 14
            end
        )
    ]).

-spec find_marker(binary(), pos_integer()) -> pos_integer().
find_marker(Signal, N) -> find_marker(Signal, N, 0, #{}).

-spec find_marker(binary(), pos_integer(), non_neg_integer(), freq_maps:t(byte())) -> pos_integer().
find_marker(Signal, N, I, Freqs) ->
    case maps:size(Freqs) of
        M when M == N ->
            %% Characters in the state map are N different characters,
            %% found the marker.
            I;
        _ ->
            %% Increase the number of next char() in state.
            Freqs2 = freq_maps:increase(binary:at(Signal, I), Freqs),
            %% Decrease the number of previous char() in state.
            Freqs3 =
                case I >= N of
                    true -> freq_maps:decrease(binary:at(Signal, I - N), Freqs2);
                    false -> Freqs2
                end,
            %% Look further.
            find_marker(Signal, N, I + 1, Freqs3)
    end.
