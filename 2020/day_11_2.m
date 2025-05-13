%% -*- mode: mercury -*-
:- module day_11_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module array2d.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.

:- import_module day_11.

main(!IO) :-
    input_seat_layout(Result, !IO),
    ( Result = ok(Seats),
      simulate(num_occupied_visible(sight_map(Seats)), 4, Seats, FinalSeats),
      N = length(filter(unify(yes(occupied)), condense(lists(FinalSeats)))),
      write_int(N, !IO), nl(!IO)
    ; Result = error(Error),
      error(error_message(Error))
    ).
