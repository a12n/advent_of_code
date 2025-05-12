%% -*- mode: mercury -*-
:- module grid.plane.
:- interface.

:- import_module list.

%% Position on 2D cartesian grid.
:- type pos ---> pos(int, int).
%% :- type vec ---> vec(int, int).

%% Grid extent as [Begin, End) positions.
:- type extent ---> extent(pos, pos).

%% Iterate over all extent positions.
:- pred foldl(pred(pos, A, A), extent, A, A).
:- mode foldl(pred(in, in, out) is det, in, in, out) is det.
:- mode foldl(pred(in, in, out) is semidet, in, in, out) is semidet.

%% True if position is within grid extent.
:- pred in_bounds(extent::in, pos::in) is semidet.

%% Moore neighborhood of a position.
:- func moore_neighbors(pos) = list(pos).

%% Von Neumann neighborhood of a position.
:- func neighbors(pos) = list(pos).

:- implementation.

:- import_module int.

foldl(Pred, extent(Pos, _) @ Extent, !Accum) :-
    foldl(Pred, Extent, Pos, !Accum).

:- pred foldl(pred(pos, A, A), extent, pos, A, A).
:- mode foldl(in(pred(in, in, out) is det), in, in, in, out) is det.
:- mode foldl(in(pred(in, in, out) is semidet), in, in, in, out) is semidet.
foldl(Pred, extent(pos(X0, _), pos(Xn, Yn)) @ Extent, pos(X, Y) @ Pos, !Accum) :-
    ( Y = Yn -> true
    ; X = Xn -> foldl(Pred, Extent, pos(X0, Y + 1), !Accum)
    ; Pred(Pos, !Accum), foldl(Pred, Extent, pos(X + 1, Y), !Accum)
    ).

in_bounds(extent(pos(X0, Y0), pos(Xn, Yn)), pos(X, Y)) :-
    X >= X0, X < Xn,
    Y >= Y0, Y < Yn.

moore_neighbors(pos(X, Y) @ Pos) =
    neighbors(Pos) ++ [
        pos(X - 1, Y - 1),
        pos(X + 1, Y - 1),
        pos(X - 1, Y + 1),
        pos(X + 1, Y + 1)
    ].

neighbors(pos(X, Y)) =
    [ pos(    X, Y - 1),
      pos(X - 1,     Y),
      pos(X + 1,     Y),
      pos(    X, Y + 1) ].
