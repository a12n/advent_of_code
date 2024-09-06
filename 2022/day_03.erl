-module(day_03).

-include_lib("eunit/include/eunit.hrl").

-type item() :: 1..26 | 27..52.
-type compartment() :: #{item() := non_neg_integer()}.
-type rucksack() :: {compartment(), compartment()}.

-export([main/1]).

main(Part) ->
    Rucksacks = lists:map(fun parse_rucksack/1, advent:file_lines(standard_io)),
    Groups =
        case Part of
            1 ->
                lists:map(fun shared_items/1, Rucksacks);
            2 ->
                [
                    shared_items([all_items(Elf1), all_items(Elf2), all_items(Elf3)])
                 || {Elf1, Elf2, Elf3} <- groups_of_three(Rucksacks)
                ]
        end,
    Sum = lists:sum([lists:sum(maps:keys(Items)) || Items <- Groups]),
    io:format("~b~n", [Sum]).

%%--------------------------------------------------------------------
%% Rucksack functions.
%%--------------------------------------------------------------------

-spec all_items(rucksack() | nonempty_list(compartment())) -> compartment().
all_items([Compartment1 | Compartments]) ->
    lists:foldl(
        fun(Items, Result) ->
            maps:merge_with(fun(_, N, M) -> N + M end, Items, Result)
        end,
        Compartment1,
        Compartments
    );
all_items({Compartment1, Compartment2}) ->
    all_items([Compartment1, Compartment2]).

-spec shared_items(rucksack() | nonempty_list(compartment())) -> compartment().
shared_items([Compartment1 | Compartments]) ->
    lists:foldl(
        fun(Items, Result) ->
            maps:intersect_with(fun(_, N, M) -> N + M end, Items, Result)
        end,
        Compartment1,
        Compartments
    );
shared_items({Compartment1, Compartment2}) ->
    shared_items([Compartment1, Compartment2]).

%%--------------------------------------------------------------------
%% Parsing functions.
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

%%--------------------------------------------------------------------
%% Part 2 functions.
%%--------------------------------------------------------------------

-spec groups_of_three(nonempty_list(rucksack())) ->
    nonempty_list({rucksack(), rucksack(), rucksack()}).
groups_of_three([]) ->
    [];
groups_of_three([Elf1, Elf2, Elf3 | Rucksacks]) ->
    [{Elf1, Elf2, Elf3} | groups_of_three(Rucksacks)].
