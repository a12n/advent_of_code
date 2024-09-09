-module(day_10).

-define(SCREEN_WIDTH, 40).
-define(SCREEN_HEIGHT, 6).

-type cpu_trace() :: [{_PC :: non_neg_integer(), _X :: integer()}].
-type crt_pos() :: 0..(?SCREEN_WIDTH * ?SCREEN_HEIGHT - 1).
-type instruction() :: {addx, integer()} | noop.

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
    Trace = cpu_trace(Program),
    case Part of
        1 ->
            SignalStrength = signal_strength(Trace, [20, 60, 100, 140, 180, 220]),
            io:format(<<"~b~n">>, [lists:sum(SignalStrength)]);
        2 ->
            LitPixels = maps:from_list([{pixel_pos(Pos), $#} || Pos <- lit_pixels(Trace)]),
            io:format(grids:to_iodata(LitPixels, {?SCREEN_HEIGHT, ?SCREEN_WIDTH}))
    end.

-spec cpu_trace([instruction()]) -> cpu_trace().
cpu_trace(Program) -> [{0, 1} | cpu_trace(Program, 0, 1)].

-spec cpu_trace([instruction()], non_neg_integer(), integer()) -> cpu_trace().
cpu_trace([], _, _) ->
    [];
cpu_trace([noop | Program], PC, X) ->
    cpu_trace(Program, PC + 1, X);
cpu_trace([{addx, Y} | Program], PC, X) ->
    PC2 = PC + 2,
    X2 = X + Y,
    [{PC2, X2} | cpu_trace(Program, PC2, X2)].

-spec signal_strength(cpu_trace(), [non_neg_integer()]) -> [integer()].
signal_strength(_, []) ->
    [];
signal_strength([{PC0, X0} | Trace = [{PC1, _} | _]], [T | Points]) when T > PC0, T =< PC1 ->
    [X0 * T | signal_strength(Trace, Points)];
signal_strength([_ | Trace], Points) ->
    signal_strength(Trace, Points).

-spec lit_pixels(cpu_trace()) -> [crt_pos()].
lit_pixels(Trace) -> lit_pixels(Trace, 0, 0).

-spec lit_pixels(cpu_trace(), non_neg_integer(), crt_pos()) -> [crt_pos()].
lit_pixels([], _, _) ->
    [];
lit_pixels([{PC0, X0} | Trace], PC, CRT) ->
    %% TODO
    _ = Trace,
    _ = PC,
    _ = CRT,
    [].

-spec pixel_pos(crt_pos()) -> grids:pos().
pixel_pos(Pos) ->
    {(Pos div ?SCREEN_WIDTH) + 1, (Pos rem ?SCREEN_WIDTH) + 1}.
