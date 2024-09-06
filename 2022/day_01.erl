#!/usr/bin/env escript

-module(day_01_1).

-type food() :: pos_integer().
-type inventory() :: nonempty_list(food()).
-export_type([food/0, inventory/0]).

-export([main/1]).

-spec parse_food(binary()) -> food().
parse_food(Number) ->
    binary_to_integer(string:trim(Number)).

-spec read_inventory(io:device()) -> inventory().
read_inventory(File) ->
    case io:get_line(File, <<>>) of
        eof -> [];
        <<$\n>> -> [];
        <<Line/bytes>> -> [parse_food(Line) | read_inventory(File)]
    end.

-spec read_inventory_list(io:device()) -> nonempty_list(inventory()).
read_inventory_list(File) ->
    case read_inventory(File) of
        [] -> [];
        Inventory -> [Inventory | read_inventory_list(File)]
    end.

main(_) ->
    io:setopts([binary, {encoding, utf8}]),
    Inventories = lists:map(fun lists:sum/1, read_inventory_list(standard_io)),
    case filename:basename(escript:script_name(), ".erl") of
        "day_01_1" ->
            io:format(<<"~B~n">>, [lists:max(Inventories)]);
        "day_01_2" ->
            {TopThree, _} = lists:split(3, lists:sort(fun erlang:'>='/2, Inventories)),
            io:format(<<"~p~n">>, [lists:sum(TopThree)])
    end.
