%% -*- mode: mercury -*-
:- module day_05_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int, ranges, string.
:- import_module day_05, io_ext.

main(!IO) :-
    lines_foldl(
        (pred(Line::in, PrevSeats::in, NextSeats::out) is semidet :-
             seat_id_string(chomp(Line), ID),
             insert(ID, PrevSeats, NextSeats)
        ), empty, Result, !IO),
    ( Result = ok(OccupiedSeats),
      ( least(OccupiedSeats, Least), greatest(OccupiedSeats, Greatest),
        prune_to_next_non_member(OccupiedSeats, _, Least, MySeat),
        prune_to_prev_non_member(OccupiedSeats, _, Greatest, MySeat) ->
        write_int(MySeat, !IO), nl(!IO)
      ; error_exit(1, "No solution", !IO)
      )
    ; Result = error(_, Error),
      error_exit(1, error_message(Error), !IO)
    ).
