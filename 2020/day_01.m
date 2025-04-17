%% -*- mode: mercury -*-
:- module day_01.
:- interface.

:- import_module int, io, list.

:- pred read_input(io.res(list(int))::out, io::di, io::uo) is det.
:- pred sums_to(int::in, list(int)::in, int::out, int::out) is cc_nondet.
:- pred sums_to(int::in, list(int)::in, int::out, int::out, int::out) is cc_nondet.

:- implementation.

:- import_module string.
:- import_module io_ext.

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

sums_to(Sum, Numbers, First, Second) :-
    member_index0(N, Numbers, I),
    member_index0(M, Numbers, J), J \= I,
    N + M = Sum,
    First = N, Second = M.

sums_to(Sum, Numbers, First, Second, Third) :-
    member_index0(N, Numbers, I),
    member_index0(M, Numbers, J), J \= I,
    member_index0(L, Numbers, K), K \= I, K \= J,
    N + M + L = Sum,
    First = N, Second = M, Third = L.
