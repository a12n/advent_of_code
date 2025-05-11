%% -*- mode: mercury -*-
:- module day_01_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module int, list, string.
:- import_module require.
:- import_module day_01, io_ext.

main(!IO) :-
    read_input(ReadResult, !IO),
    ( ReadResult = ok(Numbers),
      ( sums_to(2020, Numbers, N, M, L) ->
        write_int(N * M * L, !IO), nl(!IO)
      ; error("No solution")
      )
    ; ReadResult = error(Error),
      error(error_message(Error))
    ).
