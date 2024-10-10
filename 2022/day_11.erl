%% Monkey in the Middle
-module(day_11).

-include_lib("eunit/include/eunit.hrl").

-export([main/1]).

-spec main(1..2) -> ok.
main(Part) ->
    %% Parse input.
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
        ),
        _WorryDivider =
            case Part of
                1 -> 3;
                2 -> 1
            end
    ),
    %% Run simulation.
    {Rounds, Modulo} =
        case Part of
            1 ->
                {20, undefined};
            2 ->
                {10000,
                    lists_ext:product([Divisor || #{divisor := Divisor} <- maps:values(Monkeys)])}
        end,
    Monkeys2 = monkey_business(Monkeys, Rounds, Modulo),
    %% Print results.
    [NumInspected1, NumInspected2 | _] = lists:sort(
        fun erlang:'>='/2,
        lists:map(fun(#{inspected := NumInspected}) -> NumInspected end, maps:values(Monkeys2))
    ),
    io:format(<<"~b~n">>, [NumInspected1 * NumInspected2]).

-spec parse_monkeys([{binary(), binary()}], pos_integer()) -> map().
parse_monkeys(Lines, WorryDivider) ->
    parse_monkeys(Lines, WorryDivider, #{}).

-spec parse_monkeys([{binary(), binary()}], pos_integer(), map()) -> map().
parse_monkeys([], _, Monkeys) ->
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
    WorryDivider,
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
                        fun(Old) -> Op(Old, Old) div WorryDivider end;
                    {<<"old">>, _} ->
                        B = binary_to_integer(BStr),
                        fun(Old) -> Op(Old, B) div WorryDivider end;
                    {_, <<"old">>} ->
                        A = binary_to_integer(AStr),
                        fun(Old) -> Op(A, Old) div WorryDivider end
                end
        end,
    %% XXX: In the input, divisors are always prime numbers.
    Divisor = binary_to_integer(DivisorStr),
    TrueID = binary_to_integer(TrueIDStr),
    FalseID = binary_to_integer(FalseIDStr),
    Next = fun
        (Item) when Item rem Divisor == 0 -> TrueID;
        (_) -> FalseID
    end,
    parse_monkeys(
        Lines,
        WorryDivider,
        Monkeys#{
            ID =>
                #{
                    items => Items,
                    inspected => 0,
                    divisor => Divisor,
                    operation => Operation,
                    next => Next
                }
        }
    ).

-spec monkey_business(map(), non_neg_integer(), pos_integer() | undefined) -> map().
monkey_business(Monkeys, 0, _) ->
    Monkeys;
monkey_business(Monkeys0, Rounds, Modulo) ->
    monkey_business(
        lists:foldl(
            fun(ID, Monkeys) ->
                one_monkey_business(ID, Monkeys, Modulo)
            end,
            Monkeys0,
            lists:sort(maps:keys(Monkeys0))
        ),
        Rounds - 1,
        Modulo
    ).

-spec one_monkey_business(non_neg_integer(), map(), pos_integer() | undefined) -> map().
one_monkey_business(ThisID, Monkeys, Modulo) ->
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
        InspectedItem =
            case Modulo of
                undefined -> Inspect(Item);
                _ -> Inspect(Item) rem Modulo
            end,
        NextID = Next(InspectedItem),
        #{NextID := NextMonkey = #{items := NextItems}} = Monkeys,
        one_monkey_business(
            ThisID,
            Monkeys#{
                ThisID := ThisMonkey#{
                    items := queue:drop(Items),
                    inspected := NumInspected + 1
                },
                NextID := NextMonkey#{
                    items := queue:in(InspectedItem, NextItems)
                }
            },
            Modulo
        )
    catch
        error:empty ->
            %% This monkey don't have items, end of round for this
            %% monkey.
            Monkeys
    end.
