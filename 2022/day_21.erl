%% Monkey Math
-module(day_21).

-export([main/1]).

-spec main(1..2) -> ok.
main(Part) ->
    Env = maps:from_list(lists:map(fun parse_monkey/1, io_ext:read_lines(standard_io))),
    io:format(standard_error, "Env = ~p.~n", [Env]),
    io:format(<<"~b~n">>, [
        case Part of
            1 ->
                eval(maps:get(<<"root">>, Env), Env);
            2 ->
                {_, Left, Right} = maps:get(<<"root">>, Env),
                io:format(standard_error, <<"Left ~p, Right ~p~n">>, [Left, Right]),
                Expr = expand(Left, <<"humn">>, Env),
                Number = expand(Right, <<>>, Env),
                io:format(standard_error, <<"Expr ~p, Number ~p~n">>, [Expr, Number]),
                solve(Expr, Number, <<"humn">>)
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

-spec expand(expr(), binary(), expr_env()) -> expr().
expand(N, Var, _) when is_binary(N), N == Var -> N;
expand(N, Var, Env) when is_binary(N) -> expand(maps:get(N, Env), Var, Env);
expand(N, _, _) when is_integer(N) -> N;
expand(Expr = {_, N, M}, _, Env) when is_integer(N), is_integer(M) -> eval(Expr, Env);
expand({Op, N, M}, Var, Env) when is_binary(N), N /= Var ->
    expand({Op, expand(N, Var, Env), M}, Var, Env);
expand({Op, N, M}, Var, Env) when is_binary(M), M /= Var ->
    expand({Op, N, expand(M, Var, Env)}, Var, Env);
expand(Expr = {_, _, _}, _, _) ->
    Expr.

-spec solve(expr(), integer(), binary()) -> integer().
solve(N, K, Var) when is_binary(N), N == Var, is_integer(K) -> K;
solve({'+', N, M}, K, Var) when is_integer(K) ->
    if
        is_integer(N) -> solve(M, K - N, Var);
        is_integer(M) -> solve(N, K - M, Var)
    end;
solve({'-', N, M}, K, Var) when is_integer(K) ->
    if
        is_integer(N) -> solve(M, -(K - N), Var);
        is_integer(M) -> solve(N, K + M, Var)
    end;
solve({'*', N, M}, K, Var) when is_integer(K) ->
    if
        is_integer(N) -> solve(M, K div N, Var);
        is_integer(M) -> solve(N, K div M, Var)
    end;
solve({'/', N, M}, K, Var) when is_integer(K) ->
    if
        is_integer(N) -> solve(M, N div K, Var);
        is_integer(M) -> solve(N, M * K, Var)
    end.
