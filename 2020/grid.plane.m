%% -*- mode: mercury -*-
:- module grid.plane.
:- interface.

:- import_module list.

%% Position on 2D cartesian grid.
:- type pos ---> pos(int, int).
%% :- type vec ---> vec(int, int).

%% Grid extent as [Begin, End) positions.
:- type extent ---> extent(pos, pos).

%% True if position is within grid extent.
:- pred in_bounds(extent::in, pos::in) is semidet.

%% Moore neighborhood of a position.
:- func moore_neighbors(pos) = list(pos).

%% Von Neumann neighborhood of a position.
:- func neighbors(pos) = list(pos).

:- implementation.

:- import_module int.

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
