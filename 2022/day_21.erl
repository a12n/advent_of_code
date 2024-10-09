-module(day_21).

-export([main/1]).

-type monkey_id() :: binary().
-type operation() :: fun((number(), number()) -> number()).
-type monkey_descr() :: {
    monkey_id(),
    number()
    | {operation(), monkey_id() | number(), monkey_id() | number()}
}.

-spec main(1..2) -> ok.
main(Part) ->
    %% Parse monkey definitions.
    MonkeyDescrs = lists:map(fun parse_monkey/1, io_ext:read_lines(standard_io)),
    io:format(standard_error, "MonkeyDescrs ~p~n", [MonkeyDescrs]),
    io:format(<<"~b~n">>, [
        case Part of
            1 -> eval_monkeys(MonkeyDescrs);
            2 -> solve_monkeys(MonkeyDescrs)
        end
    ]).

-spec parse_monkey(binary()) -> monkey_descr().
parse_monkey(Line) ->
    [ID | Descr] = binary:split(Line, [<<" ">>, <<":">>], [global, trim_all]),
    case Descr of
        [Number] ->
            {ID, binary_to_integer(Number)};
        [LeftID, <<Op>>, RightID] ->
            {ID, {
                case Op of
                    $+ -> fun erlang:'+'/2;
                    $- -> fun erlang:'-'/2;
                    $* -> fun erlang:'*'/2;
                    $/ -> fun erlang:'div'/2
                end,
                LeftID,
                RightID
            }}
    end.

%%--------------------------------------------------------------------
%% Expressions and equations.
%%--------------------------------------------------------------------

-type expr() :: binary() | integer() | {operation(), expr(), expr()}.
-type expr_env() :: #{binary() => expr()}.
-type operation() :: '*' | '+' | '-' | '/'.

-spec eval(expr()) -> integer().
eval(Expr) -> eval(Expr, #{}).

-spec eval(expr(), expr_env()) -> integer().
eval(N, _) when is_integer(N) -> N;
eval(Var, Env) when is_binary(Var) -> eval(maps:get(Var, Env), Env);
eval({Op, N, M}, _) when is_integer(N), is_integer(M) ->
    case Op of
        '*' -> N * M;
        '+' -> N + M;
        '-' -> N - M;
        '/' -> N div M
    end;
eval({Op, N, M}, Env) when is_tuple(N); is_binary(N) -> eval({Op, eval(N, Env), M}, Env);
eval({Op, N, M}, Env) when is_tuple(M); is_binary(M) -> eval({Op, N, eval(M, Env)}, Env).

%%--------------------------------------------------------------------
%% Monkey processes
%%--------------------------------------------------------------------

-type monkey_state() ::
    {yell, number()} | {calculate, operation(), number() | binary(), number() | binary()}.

-spec eval_monkeys([monkey_descr()]) -> ok.
eval_monkeys(MonkeyDescrs) ->
    %% Spawn monkey processes.
    {ok, Group} = pg:start_link(),
    pg:join(
        monkeys,
        [
            self()
            | lists:map(
                fun
                    ({ID, {Fun, Left, Right}}) ->
                        spawn_link(fun() -> monkey(ID, {init, {calculate, Fun, Left, Right}}) end);
                    ({ID, Number}) ->
                        spawn_link(fun() -> monkey(ID, {init, {yell, Number}}) end)
                end,
                MonkeyDescrs
            )
        ]
    ),
    %% Awake all monkeys.
    lists:foreach(fun(PID) -> PID ! init end, pg:get_local_members(monkeys)),
    %% Wait result.
    Result =
        (fun WaitAns() ->
            receive
                {result, <<"root">>, Ans} -> Ans;
                {result, _, _} -> WaitAns()
            end
        end)(),
    exit(Group, normal),
    Result.

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

%%--------------------------------------------------------------------
%% Solve for humn
%%--------------------------------------------------------------------

-spec solve_monkeys([monkey_descr()]) -> number().
solve_monkeys(MonkeyDescrs) ->
    %% TODO
    0.
