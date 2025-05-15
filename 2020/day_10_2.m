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
        Num = num_arrangements([0 | SortedJolts]),
        write_int64(Num, !IO), nl(!IO)
      ; error("Invalid input")
      )
    ; Result = error(Error),
      error(error_message(Error))
    ).

:- func num_arrangements(list(int)) = int64.
:- pragma memo(num_arrangements/1).
num_arrangements(Jolts) = Num :-
    ( Jolts = [] -> Num = 1i64
    ; Jolts = [_] -> Num = 1i64
    ; Jolts = [J0, J1 | Jolts2], (J1 - J0) < 4 ->
      Num = num_arrangements([J1 | Jolts2]) +
            ( Jolts2 = [J2 | Jolts3], (J2 - J0) < 4 ->
              num_arrangements(Jolts2) +
              ( Jolts3 = [J3 | _], (J3 - J0) < 4 ->
                num_arrangements(Jolts3)
              ; 0i64
              )
            ; 0i64
            )
    ; Num = 0i64
    ).
