%% -*- mode: mercury -*-
:- module day_10_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module int64.
:- import_module list.
:- import_module require.
:- import_module string.

:- import_module io_ext.

main(!IO) :-
    input_string_lines(Result, !IO),
    ( Result = ok(Lines),
      ( map(to_int, map(chomp, Lines), Jolts) ->
        sort(Jolts, SortedJolts),
        Num = num_arrangements(0, SortedJolts),
        write_int64(Num, !IO), nl(!IO)
      ; error("Invalid input")
      )
    ; Result = error(Error),
      error(error_message(Error))
    ).

:- func num_arrangements(int, list(int)) = int64.
:- pragma memo(num_arrangements/2).
num_arrangements(J, Jolts0) =
    ( Jolts0 = [] -> 1i64
    ; Jolts0 = [J1 | Jolts1], (J1 - J) < 4 ->
      num_arrangements(J1, Jolts1) +
      ( Jolts1 = [J2 | Jolts2], (J2 - J) < 4 ->
        num_arrangements(J2, Jolts2) +
        ( Jolts2 = [J3 | Jolts3], (J3 - J) < 4 ->
          num_arrangements(J3, Jolts3)
        ; 0i64 )
      ; 0i64 )
    ; 0i64 ).
