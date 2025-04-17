%% -*- mode: mercury -*-
:- module day_01_1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module int, list, string.
:- import_module io_ext.

main(!IO) :-
    read_input(ReadResult, !IO),
    ( ReadResult = ok(Numbers),
      ( sums_to(2020, Numbers, N, M) ->
        write_int(N * M, !IO), nl(!IO)
      ; error_exit(1, "no solution", !IO)
      )
    ; ReadResult = error(Error),
      error_exit(1, error_message(Error), !IO)
    ).

:- pred read_input(io.res(list(int))::out, io::di, io::uo) is det.
read_input(Result, !IO) :-
    read_lines_as_strings(ReadResult, !IO),
    ( ReadResult = ok(Lines),
      ( map(to_int, map(rstrip, Lines), Numbers) ->
        Result = ok(Numbers)
      ; Result = error(make_io_error("invalid integer"))
      )
    ; ReadResult = error(Error),
      Result = error(Error)
    ).

:- pred sums_to(int::in, list(int)::in, int::out, int::out) is cc_nondet.
sums_to(Sum, Numbers, First, Second) :-
    ( member_index0(N, Numbers, I),
      member_index0(M, Numbers, J),
      I \= J, N + M = Sum ->
      First = N, Second = M
    ; fail
    ).
