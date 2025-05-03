%% -*- mode: mercury -*-
:- module day_05_1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int, string.
:- import_module day_05, io_ext.

main(!IO) :-
    lines_foldl(
        (pred(Line::in, PrevMax::in, NextMax::out) is semidet :-
             seat_id_string(chomp(Line), ID),
             NextMax = max(PrevMax, ID)
        ), 0, Result, !IO),
    ( Result = ok(MaxID),
      write_int(MaxID, !IO), nl(!IO)
    ; Result = error(_, Error),
      error_exit(1, error_message(Error), !IO)
    ).
