-module(day_11).

-include_lib("eunit/include/eunit.hrl").

-export([main/1]).

-spec main(1..2) -> ok.
main(1) ->
    Monkeys = parse_monkeys(
        lists:filtermap(
            fun
                (<<>>) ->
                    false;
                (<<Line/bytes>>) ->
                    [Key, Value] = binary:split(Line, <<":">>),
                    {true, {string:trim(Key), string:trim(Value)}}
            end,
            io_ext:read_lines(standard_io)
        )
    ),
    ?debugFmt("Monkeys ~p", [Monkeys]),
    ok.

-spec parse_monkeys([{binary(), binary()}]) -> map().
parse_monkeys(Lines) ->
    parse_monkeys(Lines, #{}).

-spec parse_monkeys([{binary(), binary()}], map()) -> map().
parse_monkeys([], Monkeys) ->
    Monkeys;
parse_monkeys(
    [
        {<<"Monkey ", IDStr/bytes>>, <<>>},
        {<<"Starting items">>, ItemsStr},
        {<<"Operation">>, OperationStr},
        {<<"Test">>, <<"divisible by ", DivisorStr/bytes>>},
        {<<"If true">>, <<"throw to monkey ", TrueIDStr/bytes>>},
        {<<"If false">>, <<"throw to monkey ", FalseIDStr/bytes>>}
        | Lines
    ],
    Monkeys
) ->
    ID = binary_to_integer(IDStr),
    Items = queue:from_list([
        binary_to_integer(string:trim(ItemStr))
     || ItemStr <- binary:split(ItemsStr, <<",">>, [global])
    ]),
    Operation =
        case binary:split(OperationStr, <<" ">>, [global]) of
            [<<"new">>, <<"=">>, AStr, OpStr, BStr] ->
                Op =
                    case OpStr of
                        <<"+">> -> fun erlang:'+'/2;
                        <<"*">> -> fun erlang:'*'/2
                    end,
                case {AStr, BStr} of
                    {<<"old">>, <<"old">>} ->
                        fun(Old) -> Op(Old, Old) div 3 end;
                    {<<"old">>, _} ->
                        B = binary_to_integer(BStr),
                        fun(Old) -> Op(Old, B) div 3 end;
                    {_, <<"old">>} ->
                        A = binary_to_integer(AStr),
                        fun(Old) -> Op(A, Old) div 3 end
                end
        end,
    Divisor = binary_to_integer(DivisorStr),
    TrueID = binary_to_integer(TrueIDStr),
    FalseID = binary_to_integer(FalseIDStr),
    Next = fun
        (Item) when Item rem Divisor == 0 -> TrueID;
        (_) -> FalseID
    end,
    parse_monkeys(
        Lines,
        Monkeys#{
            ID =>
                #{
                    items => Items,
                    inspected => 0,
                    operation => Operation,
                    next => Next
                }
        }
    ).

-spec monkey_business(map(), non_neg_integer()) -> map().
monkey_business(Monkeys, 0) ->
    Monkeys;
monkey_business(Monkeys, Rounds) ->
    monkey_business(
        lists:foldl(
            fun one_monkey_business/2,
            Monkeys,
            lists:sort(maps:keys(Monkeys))
        ),
        Rounds - 1
    ).

-spec one_monkey_business(non_neg_integer(), map()) -> map().
one_monkey_business(ThisID, Monkeys) ->
    #{
        ThisID := ThisMonkey = #{
            items := Items,
            inspected := NumInspected,
            operation := Inspect,
            next := Next
        }
    } = Monkeys,
    try
        Item = queue:get(Items),
        InspectedItem = Inspect(Item),
        ThatID = Next(InspectedItem),
        #{ThatID := ThatMonkey = #{items := ThatItems}} = Monkeys,
        one_monkey_business(
            ThisID,
            Monkeys#{
                ThisID := ThisMonkey#{
                    items := queue:drop(Items),
                    inspected := NumInspected + 1
                },
                ThatID := ThatMonkey#{
                    items := queue:in(InspectedItem, ThatItems)
                }
            }
        )
    catch
        error:empty ->
            %% This monkey don't have items, end of round for this
            %% monkey.
            Monkeys
    end.
