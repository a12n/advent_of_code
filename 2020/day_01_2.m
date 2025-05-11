%% -*- mode: mercury -*-
:- module day_01_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module require.

:- import_module io_ext.

main(!IO) :-
    input_int_list(ReadResult, !IO),
    ( ReadResult = ok(Numbers),
      ( member(N, Numbers), member(M, Numbers), member(L, Numbers), N + M + L = 2020 ->
        write_int(N * M * L, !IO), nl(!IO)
      ; error("No solution")
      )
    ; ReadResult = error(Error),
      error(error_message(Error))
    ).
