%% -*- mode: mercury -*-
:- module day_08_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool, solutions.
:- import_module day_08, io_ext.

:- pred exec(program::in, int::out) is nondet.
exec(Program, Acc) :- exec_program_nondet(Program, yes, 0, Acc, 0, _).

main(!IO) :-
    program_input(ReadResult, Program, !IO),
    ( ReadResult = ok,
      %% TODO
      solutions(exec(Program), Solutions),
      print_line(Solutions, !IO)
    ; ReadResult = error(Error),
      error_exit(1, error_message(Error), !IO)
    ).
