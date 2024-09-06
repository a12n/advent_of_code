-module(day_01).

-type food() :: pos_integer().
-type inventory() :: nonempty_list(food()).

-export([main/1]).

-spec main(1..2) -> ok.
main(Part) ->
    Inventories = lists:map(fun lists:sum/1, read_inventory_list(standard_io)),
    case Part of
        1 ->
            io:format(<<"~b~n">>, [lists:max(Inventories)]);
        2 ->
            {TopThree, _} = lists:split(3, lists:sort(fun erlang:'>='/2, Inventories)),
            io:format(<<"~b~n">>, [lists:sum(TopThree)])
    end.

%%--------------------------------------------------------------------
%% Internal functions.
%%--------------------------------------------------------------------

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
