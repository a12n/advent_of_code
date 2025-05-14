%% -*- mode: mercury -*-
:- module day_13_1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module require.

:- import_module advent.
:- import_module day_13.

main(!IO) :-
    input(Result, !IO),
    ( Result = ok(Arrive - MaybeBuses),
      filter_map(maybe_is_yes, MaybeBuses, Buses),
      ( (BusID - WaitTime) = (
            Buses >>
            map(departure(Arrive)) >>
            map(plus(-Arrive)) >>
            map_corresponding(pair, Buses) >>
            reduce(min_wait_time)
        ) ->
        write_int(BusID * WaitTime, !IO), nl(!IO)
      ; error("No solution")
      )
    ; Result = error(Error),
      error(error_message(Error))
    ).

:- func departure(int, int) = int.
departure(Arrive, Bus) = Bus * ceil_div(Arrive, Bus).

:- func ceil_div(int, int) = int.
ceil_div(N, M) = N // M + ( N rem M \= 0 -> 1; 0 ).

:- func reduce((func(T, T) = T), list(T)) = T is semidet.
reduce(F, [H | T]) = foldl(F, T, H).

:- func min_wait_time(pair(int, int), pair(int, int)) = pair(int, int).
min_wait_time((_ - WaitTimeA) @ A, (_ - WaitTimeB) @ B) = (WaitTimeA < WaitTimeB -> A; B).
