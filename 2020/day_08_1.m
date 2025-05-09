%% -*- mode: mercury -*-
:- module day_08_1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool, require.
:- import_module day_08, io_ext.

main(!IO) :-
    program_input(ReadResult, Program, !IO),
    ( ReadResult = ok,
      exec_program(Program, Halts, 0, Acc, 0, _),
      ( Halts = no ->
        write_int(Acc, !IO), nl(!IO)
      ; error("Program halts")
      )
    ; ReadResult = error(Error),
      error(error_message(Error))
    ).
