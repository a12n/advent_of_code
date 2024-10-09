-module(day_21).

-export([main/1]).

-spec main(1..2) -> ok.
main(Part) ->
    %% Parse monkey definitions.
    MonkeyDescrs = maps:from_list(lists:map(fun parse_monkey/1, io_ext:read_lines(standard_io))),
    io:format(standard_error, "MonkeyDescrs ~p~n", [MonkeyDescrs]),
    io:format(<<"~b~n">>, [
        case Part of
            1 -> eval(maps:get(<<"root">>, MonkeyDescrs), MonkeyDescrs);
            2 -> solve_monkeys(MonkeyDescrs)
        end
    ]).

-spec parse_monkey(binary()) -> {binary(), expr()}.
parse_monkey(Line) ->
    [ID | Descr] = binary:split(Line, [<<" ">>, <<":">>], [global, trim_all]),
    case Descr of
        [Number] ->
            {ID, binary_to_integer(Number)};
        [LeftID, <<Op>>, RightID] ->
            {ID, {
                case Op of
                    $+ -> '+';
                    $- -> '-';
                    $* -> '*';
                    $/ -> '/'
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

-spec solve(expr(), integer()) -> integer().
solve(Var, K) when is_binary(Var), is_integer(K) -> K;
solve({'+', N, M}, K) when is_integer(K) ->
    if
        is_integer(N) -> solve(M, K - N);
        is_integer(M) -> solve(N, K - M)
    end;
solve({'-', N, M}, K) when is_integer(K) ->
    if
        is_integer(N) -> solve(M, -(K - N));
        is_integer(M) -> solve(N, K + M)
    end;
solve({'*', N, M}, K) when is_integer(K) ->
    if
        is_integer(N) -> solve(M, K div N);
        is_integer(M) -> solve(N, K div M)
    end;
solve({'/', N, M}, K) when is_integer(K) ->
    if
        is_integer(N) -> solve(M, N div K);
        is_integer(M) -> solve(N, M * K)
    end.

%%--------------------------------------------------------------------
%% Solve for humn
%%--------------------------------------------------------------------

-spec solve_monkeys([monkey_descr()]) -> number().
solve_monkeys(MonkeyDescrs) ->
    %% TODO
    0.
