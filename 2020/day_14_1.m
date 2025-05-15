%% -*- mode: mercury -*-
:- module day_14_1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module string.

:- import_module day_14.

main(!IO) :- main(init, _, init, _, !IO).

:- pred main(mask::in, mask::out, memory::in, memory::out, io::di, io::uo) is det.
main(!Mask, !Memory, !IO) :-
    read_line_as_string(ReadResult, !IO),
    ( ReadResult = ok(Line),
      ( instr_string(Line, Instr) ->
        print_line(Instr, !IO),
        main(!Mask, !Memory, !IO)
      ; error("Invalid input")
      )
    ; ReadResult = eof,
      %% TODO
      write_uint(0u, !IO), nl(!IO)
    ; ReadResult = error(Error),
      error(error_message(Error))
    ).

:- func init = mask.
init = pair(0u, 0xF_FFFF_FFFFu).
