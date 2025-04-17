%% -*- mode: mercury -*-
:- module io_ext.
:- interface.

:- import_module io.
:- import_module list.

:- pred error_exit(int::in, string::in, io::di, io::uo) is det.
:- pred read_lines_as_strings(io.res(list(string))::out, io::di, io::uo) is det.

:- implementation.

error_exit(Status, Message, !IO) :-
    set_exit_status(Status, !IO),
    write_string(stderr_stream, Message, !IO),
    nl(!IO).

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
