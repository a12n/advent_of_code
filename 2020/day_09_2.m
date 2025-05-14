%% -*- mode: mercury -*-
:- module day_09_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module array.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module string.

:- import_module day_09.
:- import_module io_ext.

main(!IO) :-
    input_int_list(Result, !IO),
    ( Result = ok(Numbers),
      ( promise_equivalent_solutions [Invalid] invalid_number(from_list(Numbers), 25, Invalid),
        promise_equivalent_solutions [Weakness] encryption_weakness(Invalid, Numbers, Weakness) ->
        write_int(Weakness, !IO), nl(!IO)
      ; error("No solution")
      )
    ; Result = error(Error),
      error(error_message(Error))
    ).
