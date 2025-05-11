%% -*- mode: mercury -*-
:- module day_10_1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

:- import_module io_ext.

main(!IO) :-
    input_string_lines(Result, !IO),
    ( Result = ok(Lines),
      ( map(to_int, Lines, Jolts) ->
        ( sort(Jolts, SortedJolts),
          jolt_differences(SortedJolts, Diff1, Diff3) ->
          write_int(Diff1 * Diff3, !IO), nl(!IO)
        ; error("No solution")
        )
      ; error("Invalid input")
      )
    ; Result = error(Error),
      error(error_message(Error))
    ).

:- pred jolt_differences(list(int)::in, int::out, int::out) is semidet.
jolt_differences(Jolts, Diff1, Diff3) :- jolt_differences(0, Jolts, 0, Diff1, 1, Diff3).

:- pred jolt_differences(int::in, list(int)::in, int::in, int::out, int::in, int::out) is semidet.
jolt_differences(_, [], !Diff1, !Diff3).
jolt_differences(Prev, [Next | Jolts], !Diff1, !Diff3) :-
    Diff = Next - Prev,
    ( Diff = 1, !:Diff1 = !.Diff1 + 1
    ; Diff = 3, !:Diff3 = !.Diff3 + 1
    ),
    jolt_differences(Next, Jolts, !Diff1, !Diff3).
