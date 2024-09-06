-module(day_03).

-include_lib("eunit/include/eunit.hrl").

-type item() :: 1..26 | 27..52.
-type compartment() :: #{item() := non_neg_integer()}.
-type rucksack() :: {compartment(), compartment()}.

-export([main/1]).

main(1) ->
    Rucksacks = lists:map(fun parse_rucksack/1, advent:file_lines(standard_io)),
    Intersections = [
        maps:intersect_with(fun(_, N, M) -> N + M end, Compartment1, Compartment2)
     || {Compartment1, Compartment2} <- Rucksacks
    ],
    Sum = lists:sum([lists:sum(maps:keys(Items)) || Items <- Intersections]),
    io:format("~b~n", [Sum]);
main(2) ->
    io:format("Part 2~n").

%%--------------------------------------------------------------------
%% Part 1 functions.
%%--------------------------------------------------------------------

-spec parse_rucksack(binary()) -> rucksack().
parse_rucksack(ItemsStr) ->
    N = byte_size(ItemsStr),
    <<First:(N div 2)/bytes, Second:(N div 2)/bytes>> = ItemsStr,
    {parse_compartment(First), parse_compartment(Second)}.

-spec parse_compartment(binary()) -> compartment().
parse_compartment(<<>>) ->
    #{};
parse_compartment(<<C, ItemsStr/bytes>>) ->
    maps:update_with(
        parse_item(C),
        fun(N) -> N + 1 end,
        1,
        parse_compartment(ItemsStr)
    ).

-spec parse_item(byte()) -> item().
parse_item(C) when C >= $a, C =< $z -> C - $a + 1;
parse_item(C) when C >= $A, C =< $Z -> C - $A + 27.
