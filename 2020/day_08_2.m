%% -*- mode: mercury -*-
:- module day_08_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module require.
:- import_module day_08.

main(!IO) :-
    program_input(ReadResult, Program, !IO),
    ( ReadResult = ok,
      ( repair_program(Program, _, 0, Acc, 0, _) ->
        write_int(Acc, !IO), nl(!IO)
      ; error("Program failed")
      )
    ; ReadResult = error(Error),
      error(error_message(Error))
    ).
