-module(maps_ext).

-export([all/2, any/2]).

-spec all(fun(), map()) -> boolean().
all(Pred, Map) ->
    try
        maps:foreach(
            fun(Key, Value) ->
                case Pred(Key, Value) of
                    true -> true;
                    false -> throw(Key)
                end
            end,
            Map
        ),
        true
    catch
        throw:_ ->
            false
    end.

-spec any(fun(), map()) -> boolean().
any(Pred, Map) ->
    try
        maps:foreach(
            fun(Key, Value) ->
                case Pred(Key, Value) of
                    false -> false;
                    true -> throw(Key)
                end
            end,
            Map
        ),
        false
    catch
        throw:_ ->
            true
    end.
