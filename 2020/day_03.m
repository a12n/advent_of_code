%% -*- mode: mercury -*-
:- module day_03.
:- interface.

:- import_module int, io, set.

:- type extent ---> extent(int, int).
:- type pos ---> pos(int, int).
:- type slope ---> slope(int, int).
:- type tree_set == set(pos).

:- pred trees_input(res({tree_set, extent})::out, io::di, io::uo) is det.

:- func num_trees(tree_set, extent, slope) = int.
:- func num_trees(tree_set, extent, slope, pos) = int.

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

num_trees(Trees, Extent, Slope) = Num:-
    Num = num_trees(Trees, Extent, Slope, pos(0, 0)).

num_trees(Trees, Extent, Slope, Pos) = Num :-
    Num = num_trees(Trees, Extent, Slope, Pos, 0).

:- func num_trees(tree_set, extent, slope, pos, int) = int.
num_trees(Trees, extent(Rows, Cols) @ Extent, slope(Y, X) @ Slope, pos(Row, Col) @ Pos, Accum) = Num :-
    ( Row < Rows ->
      NextPos = pos(Row + Y, (Col + X) mod Cols),
      NextAccum = (member(Pos, Trees) -> Accum + 1; Accum),
      Num = num_trees(Trees, Extent, Slope, NextPos, NextAccum)
    ; Num = Accum
    ).
