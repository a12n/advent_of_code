-module(freq_maps).

-type t() :: t(term()).
-type t(Key) :: #{Key := pos_integer()}.
-export_type([t/0, t/1]).

-export([from_list/1, from_binary/1, increase/2, decrease/2]).

-spec from_list([term()]) -> t().
from_list(List) -> lists:foldl(fun increase/2, #{}, List).

-spec from_binary(binary()) -> t(byte()).
from_binary(Binary) -> from_binary(Binary, #{}).

-spec from_binary(binary(), t(byte())) -> t(byte()).
from_binary(<<>>, Freqs) -> Freqs;
from_binary(<<Byte, Binary/bytes>>, Freqs) -> from_binary(Binary, increase(Byte, Freqs)).

-spec increase(term(), t()) -> t().
increase(Key, Freqs) -> maps:update_with(Key, fun(N) -> N + 1 end, 1, Freqs).

-spec decrease(term(), t()) -> t().
decrease(Key, Freqs) ->
    try
        maps:update_with(
            Key,
            fun
                (1) -> throw(zerokey);
                (N) -> N - 1
            end,
            Freqs
        )
    catch
        throw:{badkey, _} -> Freqs;
        throw:zerokey -> maps:remove(Key, Freqs)
    end.
