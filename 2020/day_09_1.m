%% -*- mode: mercury -*-
:- module day_09_1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module array.
:- import_module maybe.
:- import_module require.
:- import_module string.

:- import_module day_09.
:- import_module io_ext.

main(!IO) :-
    get_environment_var("PREAMBLE", PreambleVar, !IO),
    ( PreambleVar = yes(PreambleStr), to_int(PreambleStr, Preamble)
    ; Preamble = 25
    ),
    input_int_list(Result, !IO),
    ( Result = ok(Numbers),
      ( weakness_number(from_list(Numbers), Preamble, N) ->
        write_int(N, !IO), nl(!IO)
      ; error("No solution")
      )
    ; Result = error(Error),
      error(error_message(Error))
    ).
