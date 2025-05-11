%% -*- mode: mercury -*-
:- module array2d_ext.
:- interface.

:- import_module array2d.

%% XXX: map_pred type
:- pred map(pred(int, int, T, T), array2d(T), array2d(T)).
:- mode map(in(pred(in, in, in, out) is det), array2d_di, array2d_uo) is det.
%% :- mode map(in(pred(in, in, in, out) is semidet), array2d_di, array2d_uo) is semidet.

:- pred map_foldl(pred(int, int, T, T, A, A), array2d(T), array2d(T), A, A).
:- mode map_foldl(in(pred(in, in, in, out, in, out) is det), array2d_di, array2d_uo, in, out) is det.
:- mode map_foldl(in(pred(in, in, in, out, in, out) is semidet), array2d_di, array2d_uo, in, out) is semidet.

:- pred semidet_lookup(array2d(T), int, int, T).
:- mode semidet_lookup(in, in, in, out) is semidet.

%% Von Neumann neighborhood of an element in the grid.
:- pred neighbor(array2d(T)::in, int::in, int::in, T::out) is nondet.

%% Moore neighborhood of an element in the grid.
:- pred moore_neighbor(array2d(T)::in, int::in, int::in, T::out) is nondet.

:- implementation.

:- import_module array.
:- import_module int.
:- import_module unit.

map_foldl(Pred, Array0, Array, !Accum) :-
    bounds(Array0, NumRows, NumCols),
    ( NumRows * NumCols =< 0 ->
      Array = from_array(0, 0, make_empty_array)
    ; unsafe_lookup(Array0, 0, 0, Elt0),
      Pred(0, 0, Elt0, Elt, !Accum),
      map_foldl(Pred, init(NumRows, NumCols, Elt), Array, !Accum, 0, 1, NumRows, NumCols)
    ).

:- pred map_foldl(pred(int, int, T, T, A, A), array2d(T), array2d(T), A, A, int, int, int, int).
:- mode map_foldl(in(pred(in, in, in, out, in, out) is det), array2d_di, array2d_uo, in, out, in, in, in, in) is det.
:- mode map_foldl(in(pred(in, in, in, out, in, out) is semidet), array2d_di, array2d_uo, in, out, in, in, in, in) is semidet.
map_foldl(Pred, !Array, !Accum, Row, Col, NumRows, NumCols) :-
    ( Row = NumRows ->
      true
    ; Col = NumCols ->
      map_foldl(Pred, !Array, !Accum, Row + 1, 0, NumRows, NumCols)
    ; unsafe_lookup(!.Array, Row, Col, Elt0),
      Pred(Row, Col, Elt0, Elt, !Accum),
      unsafe_set(Row, Col, Elt, !Array),
      map_foldl(Pred, !Array, !Accum, Row, Col + 1, NumRows, NumCols)
    ).

map(Pred, !Array) :-
    FoldPred = (pred(Row::in, Col::in, !.Elt::in, !:Elt::out, !.Accum::in, !:Accum::out) is det :- Pred(Row, Col, !Elt)),
    map_foldl(FoldPred, !Array, unit, _).

semidet_lookup(Array, Row, Col, Elt) :-
    in_bounds(Array, Row, Col),
    unsafe_lookup(Array, Row, Col, Elt).

neighbor(Array, Row, Col, Elt) :-
    neighbor_pos(Array, Row, Col, RowAdj, ColAdj),
    unsafe_lookup(Array, RowAdj, ColAdj, Elt).

moore_neighbor(Array, Row, Col, Elt) :-
    moore_neighbor_pos(Array, Row, Col, RowAdj, ColAdj),
    unsafe_lookup(Array, RowAdj, ColAdj, Elt).

:- pred neighbor_pos(array2d(T)::in, int::in, int::in, int::out, int::out) is nondet.
neighbor_pos(Array, Row, Col, RowAdj, ColAdj) :-
    ( RowAdj = Row - 1, ColAdj =     Col, in_bounds(Array, RowAdj, ColAdj)
    ; RowAdj =     Row, ColAdj = Col - 1, in_bounds(Array, RowAdj, ColAdj)
    ; RowAdj =     Row, ColAdj = Col + 1, in_bounds(Array, RowAdj, ColAdj)
    ; RowAdj = Row + 1, ColAdj =     Col, in_bounds(Array, RowAdj, ColAdj)
    ).

:- pred moore_neighbor_pos(array2d(T)::in, int::in, int::in, int::out, int::out) is nondet.
moore_neighbor_pos(Array, Row, Col, RowAdj, ColAdj) :-
    ( neighbor_pos(Array, Row, Col, RowAdj, ColAdj)
    ; RowAdj = Row - 1, ColAdj = Col - 1, in_bounds(Array, RowAdj, ColAdj)
    ; RowAdj = Row - 1, ColAdj = Col + 1, in_bounds(Array, RowAdj, ColAdj)
    ; RowAdj = Row + 1, ColAdj = Col - 1, in_bounds(Array, RowAdj, ColAdj)
    ; RowAdj = Row + 1, ColAdj = Col + 1, in_bounds(Array, RowAdj, ColAdj)
    ).
