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
      write_uint(memory_sum(!.Memory), !IO), nl(!IO)
    ; ReadResult = error(Error),
      error(error_message(Error))
    ).
