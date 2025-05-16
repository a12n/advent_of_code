%% -*- mode: mercury -*-
:- module day_15_1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module require.
:- import_module string.

main(!IO) :-
    read_line_as_string(ReadResult, !IO),
    ( ReadResult = ok(Line),
      ( map(to_int, split_at_char(',', chomp(Line)), Numbers) ->
        %% TODO
        print_line(Numbers, !IO)
      ; error("Invalid input")
      )
    ; ReadResult = eof,
      error("EOF")
    ; ReadResult = error(Error),
      error(error_message(Error))
    ).
