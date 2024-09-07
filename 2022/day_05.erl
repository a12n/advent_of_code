-module(day_05).

-type crate() :: char().
-type stack() :: [crate()].
-type stack_map() :: #{pos_integer() := stack()}.
-type move() :: {_Count :: pos_integer(), _From :: pos_integer(), _To :: pos_integer()}.

-export([main/1, parse/1]).

-spec main(1..2) -> ok.
main(1) ->
    {Stacks, Moves} = parse(io_ext:read_lines(standard_io)),
    StacksAfterMoves = lists:foldl(fun crate_mover_9000/2, Stacks, Moves),
    TopOfEachStack = [
        TopCrate
     || {_, [TopCrate | _]} <- lists:sort(maps:to_list(StacksAfterMoves))
    ],
    io:format("~s~n", [TopOfEachStack]).

%%--------------------------------------------------------------------
%% Move functions.
%%--------------------------------------------------------------------

-spec crate_mover_9000(move(), stack_map()) -> stack_map().
crate_mover_9000({N, From, To}, Stacks) ->
    FromStack = maps:get(From, Stacks),
    ToStack = maps:get(To, Stacks),
    {Crates, LeftOnFromStack} = lists:split(N, FromStack),
    Stacks#{From := LeftOnFromStack, To := move_crates(Crates, ToStack)}.

-spec move_crates([crate()], stack()) -> stack().
move_crates([], ToStack) ->
    ToStack;
move_crates([Crate | Crates], ToStack) ->
    move_crates(Crates, [Crate | ToStack]).

%%--------------------------------------------------------------------
%% Parsing functions.
%%--------------------------------------------------------------------

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
