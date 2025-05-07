%% -*- mode: mercury -*-
:- module day_07_1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, solutions.
:- import_module day_07, io_ext.

main(!IO) :-
    read_bags(Result, !IO),
    ( Result = ok(Mapping),
      solutions(outermost(Mapping, "shiny gold"), Solutions),
      write_int(length(Solutions), !IO), nl(!IO)
    ; Result = error(Error),
      error_exit(1, error_message(Error), !IO)
    ).
