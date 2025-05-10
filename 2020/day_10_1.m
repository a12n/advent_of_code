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
    input_lines(Result, !IO),
    ( Result = ok(Lines),
      ( map(to_int, map(chomp, Lines), Jolts) ->
        ( sort(Jolts, SortedJolts),
          jolt_differences(SortedJolts, Num1, Num3) ->
          write_int(Num1 * Num3, !IO), nl(!IO)
        ; error("No solution")
        )
      ; error("Invalid input")
      )
    ; Result = error(Error),
      error(error_message(Error))
    ).

:- pred jolt_differences(list(int)::in, int::out, int::out) is semidet.
jolt_differences(Adapters, Num1, Num3) :- jolt_differences(0, Adapters, 0, Num1, 1, Num3).

:- pred jolt_differences(int::in, list(int)::in, int::in, int::out, int::in, int::out) is semidet.
jolt_differences(_, [], !Num1, !Num3).
jolt_differences(Prev, [Next | Adapters], !Num1, !Num3) :-
    Diff = Next - Prev,
    ( Diff = 1, !:Num1 = !.Num1 + 1
    ; Diff = 3, !:Num3 = !.Num3 + 1
    ),
    jolt_differences(Next, Adapters, !Num1, !Num3).
