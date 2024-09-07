-module(day_05).

-type crate() :: char().
-type stack() :: [crate()].
-type stack_map() :: #{pos_integer() := stack()}.
-type move() :: {_Count :: pos_integer(), _From :: pos_integer(), _To :: pos_integer()}.

-export([main/1, parse/1]).

-spec main(1..2) -> ok.
main(1) ->
    {Stacks, Moves} = parse(io_ext:read_lines(standard_io)),
    Stacks2 = lists:foldl(fun perform_move/2, Stacks, Moves),
    TopOfEachStack = [Crate || {_, [Crate | _]} <- lists:sort(maps:to_list(Stacks2))],
    io:format("~s~n", [TopOfEachStack]).

-spec perform_move(move(), stack_map()) -> stack_map().
perform_move(_Move = {0, _From, _To}, Stacks) ->
    %% Zero crates, nothing to do.
    Stacks;
perform_move(_Move = {_, From, _To = From}, Stacks) ->
    %% Move to the same stack, nothing to do.
    Stacks;
perform_move(_Move = {N, From, To}, Stacks) ->
    %% ?debugVal({N, From, To}),
    FromStack = maps:get(From, Stacks),
    ToStack = maps:get(To, Stacks),
    {FromStack2, ToStack2} = perform_move(N, FromStack, ToStack),
    %% ?debugVal({FromStack2, ToStack2}),
    Stacks#{From := FromStack2, To := ToStack2}.

-spec perform_move(non_neg_integer(), stack(), stack()) -> {stack(), stack()}.
perform_move(0, FromStack, ToStack) ->
    %% ?debugVal({N, FromStack, ToStack}),
    {FromStack, ToStack};
perform_move(N, [Crate | FromStackLeft], ToStack) when N > 0 ->
    %% ?debugVal({N, FromStack, ToStack}),
    perform_move(N - 1, FromStackLeft, [Crate | ToStack]).

-spec parse([binary()]) -> {stack_map(), [move()]}.
parse(Lines) ->
    #{stacks := Stacks, moves := Moves} =
        parse_stacks(Lines, #{index => 1, stacks => #{}, moves => []}),
    {maps:map(fun(_, Stack) -> lists:reverse(Stack) end, Stacks), lists:reverse(Moves)}.

-spec parse_stacks([binary()], map()) -> map().
parse_stacks([<<" 1 ", _/bytes>>, <<>> | Lines], State) ->
    parse_moves(Lines, State);
parse_stacks(
    [<<"[", Crate, "]", MaybeLine/bytes>> | Lines], State = #{index := Index, stacks := Stacks}
) ->
    Stacks2 = maps:update_with(Index, fun(Stack) -> [Crate | Stack] end, [Crate], Stacks),
    case MaybeLine of
        <<>> ->
            parse_stacks(Lines, State#{index := 1, stacks := Stacks2});
        <<" ", Line/bytes>> ->
            parse_stacks([Line | Lines], State#{index := Index + 1, stacks := Stacks2})
    end;
parse_stacks([<<"   ", MaybeLine/bytes>> | Lines], State = #{index := Index}) ->
    case MaybeLine of
        <<>> -> parse_stacks(Lines, State#{index := 1});
        <<" ", Line/bytes>> -> parse_stacks([Line | Lines], State#{index := Index + 1})
    end.

-spec parse_moves([binary()], map()) -> map().
parse_moves([], State) ->
    State;
parse_moves([Line = <<"move ", _/bytes>> | Lines], State = #{moves := Moves}) ->
    [<<"move">>, N, <<"from">>, From, <<"to">>, To] =
        binary:split(Line, <<" ">>, [global, trim]),
    Moves2 = [{binary_to_integer(N), binary_to_integer(From), binary_to_integer(To)} | Moves],
    parse_moves(Lines, State#{moves := Moves2}).
