%% -*- mode: mercury -*-
:- module day_03.
:- interface.

:- import_module int, io, set.

:- type extent ---> extent(int, int).
:- type pos ---> pos(int, int).
:- type slope ---> slope(int, int).
:- type tree_set == set(pos).

:- pred trees_input(res({tree_set, extent})::out, io::di, io::uo) is det.

:- implementation.

trees_input(Result, !IO) :-
    trees_input(pos(0, 0), 0, init, Result, !IO).

:- pred trees_input(pos::in, int::in, tree_set::in, res({tree_set, extent})::out, io::di, io::uo) is det.
trees_input(pos(Row, Col) @ Pos, MaxCol, Trees, Result, !IO) :-
    read_char_unboxed(stdin_stream, ReadResult, Char, !IO),
    ( ReadResult = ok,
      ( Char = '\n' ->
        trees_input(pos(Row + 1, 0), MaxCol, Trees, Result, !IO)
      ; Char = '#' ->
        trees_input(pos(Row, Col + 1), max(MaxCol, Col), insert(Trees, Pos), Result, !IO)
      ; Char = ('.') ->
        trees_input(pos(Row, Col + 1), max(MaxCol, Col), Trees, Result, !IO)
      ; Result = error(make_io_error("Invalid character"))
      )
    ; ReadResult = eof,
      Result = ok({Trees, extent(Row, MaxCol + 1)})
    ; ReadResult = error(Error),
      Result = error(Error)
    ).
