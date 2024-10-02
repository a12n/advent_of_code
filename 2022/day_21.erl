-module(day_21).

-export([main/1]).

-spec main(1..2) -> ok.
main(1) ->
    %% Parse monkey definitions.
    MonkeyDescrs =
        lists:map(
            fun(Line) ->
                [ID | Descr] = binary:split(Line, [<<" ">>, <<":">>], [global, trim_all]),
                case Descr of
                    [Number] ->
                        {ID, binary_to_integer(Number)};
                    [LeftID, <<Op>>, RightID] ->
                        {ID,
                            case Op of
                                $+ -> fun erlang:'+'/2;
                                $- -> fun erlang:'-'/2;
                                $* -> fun erlang:'*'/2;
                                $/ -> fun erlang:'div'/2
                            end, LeftID, RightID}
                end
            end,
            io_ext:read_lines(standard_io)
        ),
    io:format(standard_error, "MonkeyDescrs ~p~n", [MonkeyDescrs]),
    %% Spawn monkey processes.
    {ok, _} = pg:start_link(),
    pg:join(
        monkeys,
        [
            self()
            | lists:map(
                fun
                    ({ID, Number}) ->
                        spawn_link(fun() -> monkey(ID, {init, {yell, Number}}) end);
                    ({ID, Fun, Left, Right}) ->
                        spawn_link(fun() -> monkey(ID, {init, {calculate, Fun, Left, Right}}) end)
                end,
                MonkeyDescrs
            )
        ]
    ),
    %% Awake all monkeys.
    lists:foreach(fun(PID) -> PID ! init end, pg:get_local_members(monkeys)),
    %% Wait result.
    io:format(<<"~b~n">>, [
        (fun WaitAns() ->
            receive
                {result, <<"root">>, Ans} -> Ans;
                {result, _, _} -> WaitAns()
            end
        end)()
    ]).

%%--------------------------------------------------------------------
%% Monkey processes
%%--------------------------------------------------------------------

-type operation() :: fun((number(), number()) -> number()).
-type monkey_state() ::
    {yell, number()} | {calculate, operation(), number() | binary(), number() | binary()}.

-spec monkey(binary(), monkey_state() | {init, monkey_state()}) -> ok.
monkey(ID, {init, State}) ->
    receive
        init -> monkey(ID, State);
        _ -> monkey(ID, {init, State})
    end;
monkey(ID, {yell, Number}) ->
    io:format(standard_error, <<"~s: yell ~p~n">>, [ID, Number]),
    lists:foreach(fun(PID) -> PID ! {result, ID, Number} end, pg:get_local_members(monkeys));
monkey(ID, {calculate, Fun, Left, Right}) when is_number(Left), is_number(Right) ->
    io:format(standard_error, <<"~s: calculated, ~p ~p~n">>, [ID, Left, Right]),
    monkey(ID, {yell, Fun(Left, Right)});
monkey(ID, {calculate, Fun, Left, Right}) ->
    receive
        {result, Left, Number} ->
            io:format(standard_error, <<"~s: received left ~p~n">>, [ID, Number]),
            monkey(ID, {calculate, Fun, Number, Right});
        {result, Right, Number} ->
            io:format(standard_error, <<"~s: received right ~p~n">>, [ID, Number]),
            monkey(ID, {calculate, Fun, Left, Number});
        {result, _, _} ->
            monkey(ID, {calculate, Fun, Left, Right})
    end.
