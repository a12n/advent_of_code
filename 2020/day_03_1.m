%% -*- mode: mercury -*-
:- module day_03_1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module day_03, io_ext.

main(!IO) :-
    trees_input(Result, !IO),
    ( Result = ok({Trees, Extent}),
      write_int(num_trees(Trees, Extent, slope(1, 3)), !IO),
      nl(!IO)
    ; Result = error(Error),
      error_exit(1, error_message(Error), !IO)
    ).
