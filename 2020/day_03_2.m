%% -*- mode: mercury -*-
:- module day_03_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module day_03, int, io_ext.

main(!IO) :-
    trees_input(Result, !IO),
    ( Result = ok({Trees, Extent}),
      NumTrees = num_trees(Trees, Extent),
      write_int(
          NumTrees(slope(1, 1)) * NumTrees(slope(1, 3)) *
          NumTrees(slope(1, 5)) * NumTrees(slope(1, 7)) *
          NumTrees(slope(2, 1)),
          !IO
      ),
      nl(!IO)
    ; Result = error(Error),
      error_exit(1, error_message(Error), !IO)
    ).
