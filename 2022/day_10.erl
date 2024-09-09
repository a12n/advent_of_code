-module(day_10).

-type instruction() :: {addx, integer()} | noop.
-type trace() :: [{_PC :: non_neg_integer(), _X :: integer()}].

-define(SCREEN_WIDTH, 40).
-define(SCREEN_HEIGHT, 6).

-export([main/1]).

-spec main(1..2) -> ok.
main(Part) ->
    Program = lists:map(
        fun
            (<<"addx ", N/bytes>>) -> {addx, binary_to_integer(N)};
            (<<"noop">>) -> noop
        end,
        io_ext:read_lines(standard_io)
    ),
    Trace = trace(Program),
    case Part of
        1 ->
            SignalStrength = signal_strength(Trace, [20, 60, 100, 140, 180, 220]),
            io:format(<<"~b~n">>, [lists:sum(SignalStrength)]);
        2 ->
            LitPixels = maps:from_list([{pixel_pos(Pos), $#} || Pos <- lit_pixels(Trace)]),
            io:format(grids:to_iodata(LitPixels, {?SCREEN_HEIGHT, ?SCREEN_WIDTH}))
    end.

-spec trace([instruction()]) -> trace().
trace(Program) -> [{0, 1} | trace(Program, 0, 1)].

-spec trace([instruction()], non_neg_integer(), integer()) -> trace().
trace([], _, _) ->
    [];
trace([noop | Program], PC, X) ->
    trace(Program, PC + 1, X);
trace([{addx, Y} | Program], PC, X) ->
    PC2 = PC + 2,
    X2 = X + Y,
    [{PC2, X2} | trace(Program, PC2, X2)].

-spec signal_strength(trace(), [non_neg_integer()]) -> [integer()].
signal_strength(_, []) ->
    [];
signal_strength([{PC0, X0} | Trace = [{PC1, _} | _]], [T | Points]) when T > PC0, T =< PC1 ->
    [X0 * T | signal_strength(Trace, Points)];
signal_strength([_ | Trace], Points) ->
    signal_strength(Trace, Points).

-spec lit_pixels(trace()) -> [non_neg_integer()].
lit_pixels(Trace) ->
    %% TODO
    [].

-spec pixel_pos(non_neg_integer()) -> grids:pos().
pixel_pos(Pos) ->
    {(Pos div ?SCREEN_WIDTH) + 1, (Pos rem ?SCREEN_WIDTH) + 1}.
