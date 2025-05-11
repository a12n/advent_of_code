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

:- implementation.

:- import_module int.
:- import_module unit.

map_foldl(Pred, !Array, !Accum) :-
    bounds(!.Array, NumRows, NumCols),
    map_foldl(Pred, !Array, !Accum, 0, 0, NumRows, NumCols).

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
