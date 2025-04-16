%% -*- mode: mercury -*-
:- module io_ext.
:- interface.

:- import_module io.
:- import_module list.

:- pred read_lines_as_strings(io.res(list(string))::out, io::di, io::uo) is det.

:- implementation.

read_lines_as_strings(Result, !IO) :-
    read_lines_as_strings([], Result, !IO).

:- pred read_lines_as_strings(list(string)::in, io.res(list(string))::out, io::di, io::uo) is det.

read_lines_as_strings(Accum, Result, !IO) :-
    read_line_as_string(ReadResult, !IO),
    ( ReadResult = ok(Line),
      read_lines_as_strings([Line | Accum], Result, !IO)
    ; ReadResult = eof,
      Result = ok(reverse(Accum))
    ; ReadResult = error(Error),
      Result = error(Error)
    ).
