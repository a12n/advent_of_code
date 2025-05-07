%% -*- mode: mercury -*-
:- module day_07_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, solutions.
:- import_module day_07, io_ext.

main(!IO) :-
    read_bags(Result, !IO),
    ( Result = ok(Mapping),
      ( Total = contains_total(Mapping, "shiny gold") ->
        write_int(Total, !IO), nl(!IO)
      ; error_exit(1, "No solution", !IO)
      )
    ; Result = error(Error),
      error_exit(1, error_message(Error), !IO)
    ).
