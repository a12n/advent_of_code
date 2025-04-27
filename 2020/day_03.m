%% -*- mode: mercury -*-
:- module day_03.
:- interface.

:- import_module char, int, io, map.

:- type extent ---> extent(int, int).
:- type pos ---> pos(int, int).
:- type slope ---> slope(int, int).
:- type tile ---> empty; tree.
:- type grid == map(pos, tile).

:- pred tile_char(tile, char).
:- mode tile_char(in, out) is det.
:- mode tile_char(out, in) is semidet.

:- pred grid_input(res({grid, extent})::out, io::di, io::uo) is det.

:- implementation.

tile_char(empty, ('.')).
tile_char(tree, '#').

grid_input(Result, !IO) :-
    grid_input(pos(0, 0), 0, init, Result, !IO).

:- pred grid_input(pos::in, int::in, grid::in, res({grid, extent})::out, io::di, io::uo) is det.
grid_input(pos(Row, Col) @ Pos, MaxCol, Grid, Result, !IO) :-
    read_char_unboxed(stdin_stream, ReadResult, Char, !IO),
    ( ReadResult = ok,
      ( Char = '\n' ->
        grid_input(pos(Row + 1, 0), MaxCol, Grid, Result, !IO)
      ; tile_char(Tile, Char) ->
        grid_input(pos(Row, Col + 1), max(MaxCol, Col), set(Grid, Pos, Tile), Result, !IO)
      ; Result = error(make_io_error("Invalid character"))
      )
    ; ReadResult = eof,
      Result = ok({Grid, extent(Row, MaxCol + 1)})
    ; ReadResult = error(Error),
      Result = error(Error)
    ).
