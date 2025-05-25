%% -*- mode: mercury -*-
:- module day_14_1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module uint.

:- import_module day_14.

main(!IO) :- main(init_mask, _, init_memory, _, !IO).

:- pred main(mask::in, mask::out, memory::in, memory::out, io::di, io::uo) is det.
main(!Mask, !Memory, !IO) :-
    read_line_as_string(ReadResult, !IO),
    ( ReadResult = ok(Line),
      ( instr_string(Line, Instr) ->
        decoder_chip(Instr, !Mask, !Memory),
        main(!Mask, !Memory, !IO)
      ; error("Invalid input")
      )
    ; ReadResult = eof,
      foldl_values(plus, !.Memory, 0u, Sum),
      write_uint(Sum, !IO), nl(!IO)
    ; ReadResult = error(Error),
      error(error_message(Error))
    ).

%% map.foldl_values/4 requires predicate, but uint.plus/2 is a
%% function.
:- pred plus(uint, uint, uint).
:- mode plus(in, in, out) is det.
plus(A, B, plus(A, B)).
