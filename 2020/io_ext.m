%% -*- mode: mercury -*-
:- module io_ext.
:- interface.

:- import_module char.
:- import_module io.
:- import_module list.
:- import_module maybe.

:- pred error_exit(int::in, string::in, io::di, io::uo) is det.
:- pred input_chars_lines(res(list(list(char)))::out, io::di, io::uo) is det.
:- pred input_int_list(res(list(int))::out, io::di, io::uo) is det.
:- pred input_string_lines(res(list(string))::out, io::di, io::uo) is det.

:- pred lines_foldl(pred(string, T, T), T, maybe_partial_res(T), io, io).
:- mode lines_foldl((pred(in, in, out) is semidet), in, out, di, uo) is det.

:- pred lines_foldl2(pred(maybe(string), T, T, U, U), T, U, maybe_partial_res(T), io, io).
:- mode lines_foldl2((pred(in, in, out, in, out) is semidet), in, in, out, di, uo) is det.

:- implementation.

:- import_module string.

error_exit(Status, Message, !IO) :-
    set_exit_status(Status, !IO),
    write_string(stderr_stream, Message, !IO),
    nl(!IO).

input_int_list(Result, !IO) :-
    input_string_lines(ReadResult, !IO),
    ( ReadResult = ok(Lines),
      ( map(to_int, map(chomp, Lines), Numbers) ->
        Result = ok(Numbers)
      ; Result = error(make_io_error("Invalid input"))
      )
    ; ReadResult = error(Error),
      Result = error(Error)
    ).

input_chars_lines(Result, !IO) :- input_chars_lines(Result, [], !IO).

:- pred input_chars_lines(res(list(list(char)))::out, list(list(char))::in, io::di, io::uo) is det.
input_chars_lines(Result, Lines, !IO) :-
    read_line(ReadResult, !IO),
    ( ReadResult = ok(Line0),
      input_chars_lines(Result, [(split_last(Line0, Line, '\n') -> Line; Line0) | Lines], !IO)
    ; ReadResult = eof,
      Result = ok(reverse(Lines))
    ; ReadResult = error(Error),
      Result = error(Error)
    ).

input_string_lines(Result, !IO) :- input_string_lines(Result, [], !IO).

:- pred input_string_lines(res(list(string))::out, list(string)::in, io::di, io::uo) is det.
input_string_lines(Result, Lines, !IO) :-
    read_line_as_string(ReadResult, !IO),
    ( ReadResult = ok(Line),
      input_string_lines(Result, [chomp(Line) | Lines], !IO)
    ; ReadResult = eof,
      Result = ok(reverse(Lines))
    ; ReadResult = error(Error),
      Result = error(Error)
    ).

lines_foldl(Pred, Accum, Result, !IO) :-
    read_line_as_string(ReadResult, !IO),
    ( ReadResult = ok(Line),
      ( Pred(Line, Accum, Accum2) ->
        lines_foldl(Pred, Accum2, Result, !IO)
      ; Result = error(Accum, make_io_error("Predicate failed"))
      )
    ; ReadResult = eof,
      Result = ok(Accum)
    ; ReadResult = error(Error),
      Result = error(Accum, Error)
    ).

lines_foldl2(Pred, Accum, State, Result, !IO) :-
    read_line_as_string(ReadResult, !IO),
    ( ReadResult = ok(Line),
      ( Pred(yes(Line), Accum, Accum2, State, State2) ->
        lines_foldl2(Pred, Accum2, State2, Result, !IO)
      ; Result = error(Accum, make_io_error("Predicate failed"))
      )
    ; ReadResult = eof,
      ( Pred(no, Accum, Accum2, State, State2) ->
        Result = ok(Accum2)
      ; Result = error(Accum, make_io_error("Predicate failed"))
      )
    ; ReadResult = error(Error),
      Result = error(Accum, Error)
    ).
