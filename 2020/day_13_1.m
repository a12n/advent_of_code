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

:- import_module day_13.

%% Next bus of ID=19 after timestamp=939:
%% math.ceil(939/19) * 19

main(!IO) :-
    input(Result, !IO),
    ( Result = ok(Arrive - MaybeBuses),
      filter_map(maybe_is_yes, MaybeBuses, Buses),
      map((pred(Bus::in, Wait::out) is det :-
               Wait = (Bus - (departure(Arrive, Bus) - Arrive))
          ), Buses, WaitTimes),
      ( reduce((func((_ - WaitTimeA) @ A, (_ - WaitTimeB) @ B) =
                ( WaitTimeA < WaitTimeB -> A ; B )
               ), WaitTimes, EarliestBus) ->
        write_int(fst(EarliestBus) * snd(EarliestBus), !IO), nl(!IO)
      ; error("No solution")
      )
    ; Result = error(Error),
      error(error_message(Error))
    ).

:- func departure(int, int) = int.
departure(Arrive, Bus) = Bus * ceil_div(Arrive, Bus).

:- func ceil_div(int, int) = int.
ceil_div(N, M) = N // M + ( N rem M \= 0 -> 1; 0 ).

:- pred reduce((func(T, T) = T)::in, list(T)::in, T::out) is semidet.
reduce(F, [H | T], Ans) :- Ans = foldl(F, T, H).
