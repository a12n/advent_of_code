%% -*- mode: mercury -*-
:- module day_08_1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module day_08, io_ext.

main(!IO) :-
    program_input(ReadResult, Program, !IO),
    ( ReadResult = ok,
      run_program(Program, Loop, 0, Acc, 0, _),
      ( Loop = yes ->
        write_int(Acc, !IO), nl(!IO)
      ; error_exit(1, "Program terminates", !IO)
      )
    ; ReadResult = error(Error),
      error_exit(1, error_message(Error), !IO)
    ).
