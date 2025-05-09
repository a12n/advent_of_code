%% -*- mode: mercury -*-
:- module day_09_1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module array.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module string.

:- pred main_loop(int::in, array(int)::array_di, array(int)::array_uo, io::di, io::uo) is cc_multi.
main_loop(I, !Queue, !IO) :-
    read_line_as_string(ReadResult, !IO),
    ( ReadResult = ok(Line),
      ( to_int(chomp(Line), N) ->
        ( I >= size(!.Queue) ->
          ( member(!.Queue, A), member(!.Queue, B), A + B = N ->
            trace [io(!IO)] format(stderr_stream, "%d = %d + %d\n", [i(N), i(A), i(B)], !IO),
            set(I rem size(!.Queue), N, !Queue),
            main_loop(I + 1, !Queue, !IO)
          ; write_int(N, !IO), nl(!IO)
          )
        ; set(I, N, !Queue),
          main_loop(I + 1, !Queue, !IO)
        )
      ; error("Invalid input")
      )
    ; ReadResult = eof,
      error("No solution")
    ; ReadResult = error(Error),
      error(error_message(Error))
    ).

main(!IO) :-
    get_environment_var("PREAMBLE", PreambleVar, !IO),
    ( to_int(maybe_default("25", PreambleVar), Preamble) ->
      main_loop(0, init(Preamble, 0), _, !IO)
    ; error("Invalid PREAMBLE")
    ).
