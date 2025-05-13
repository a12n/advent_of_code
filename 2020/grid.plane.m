%% -*- mode: mercury -*-
:- module grid.plane.
:- interface.

:- import_module list.

%%---------------------------------------------------------------------------
%% Position and extent

%% Position (x, y) on 2D cartesian grid. -x and +x are left and right,
%% -y and +y are down and up.
:- type pos ---> pos(int, int).

%% Grid extent as [Begin, End) positions.
:- type extent ---> extent(pos, pos).

%% True if position is within grid extent.
:- pred in_bounds(extent::in, pos::in) is semidet.

%% Iterate over all extent positions.
:- pred foldl(pred(pos, A, A), extent, A, A).
%% :- mode foldl(pred(in, di,  uo) is det, in, di,  uo) is det.
:- mode foldl(pred(in, in, out) is det, in, in, out) is det.

:- pred foldl2(pred(pos, A, A, B, B), extent, A, A, B, B).
%% :- mode foldl2(pred(in, in, out, di,  uo) is det, in, in, out, di,  uo) is det.
:- mode foldl2(pred(in, in, out, in, out) is det, in, in, out, in, out) is det.

%%---------------------------------------------------------------------------
%% Directions (restricted)

%% Moore neighborhood of a position.
:- type moore_dir --->
   '↖'; '↑'; '↗';
   '←';      '→';
   '↙'; '↓'; '↘'.

%% Von Neumann neighborhood of a position.
:- type dir =< moore_dir --->
        '↑';
   '←';      '→';
        '↓'.

:- func moore_neighbor_dirs = list(vec).
:- func moore_neighbors(pos) = list(pos).

:- func neighbor_dirs = list(vec).
:- func neighbors(pos) = list(pos).

%%---------------------------------------------------------------------------
%% Directions (vectors)

:- type vec ---> vec(int, int).

:- func minus(pos, vec) = pos.
:- func minus_pos(pos, pos) = vec.
:- func plus(pos, vec) = pos.

%%---------------------------------------------------------------------------
:- implementation.

:- import_module int.
:- import_module unit.

minus(pos(X, Y), vec(Xv, Yv)) = pos(X - Xv, Y - Yv).

minus_pos(pos(Xa, Ya), pos(Xb, Yb)) = vec(Xa - Xb, Ya - Yb).

plus(pos(X, Y), vec(Xv, Yv)) = pos(X + Xv, Y + Yv).

foldl(Pred, Extent, !A) :-
    foldl2((pred(Pos::in, !.I::in, !:I::out, !.A::in, !:A::out) is det :- Pred(Pos, !A)), Extent, unit, _, !A).

foldl2(Pred, extent(Pos0, _) @ Extent, !A, !B) :- foldl2(Pred, Extent, Pos0, !A, !B).

:- pred foldl2(pred(pos, A, A, B, B), extent, pos, A, A, B, B).
%% :- mode foldl2(pred(in, in, out, di,  uo) is det, in, in, in, out, di,  uo) is det.
:- mode foldl2(pred(in, in, out, in, out) is det, in, in, in, out, in, out) is det.
foldl2(Pred, extent(pos(X0, _), pos(Xn, Yn)) @ Extent, pos(X, Y) @ Pos, !A, !B) :-
    ( Y >= Yn -> true
    ; X >= Xn -> foldl2(Pred, Extent, pos(X0, Y + 1), !A, !B)
    ; Pred(Pos, !A, !B), foldl2(Pred, Extent, pos(X + 1, Y), !A, !B)
    ).

in_bounds(extent(pos(X0, Y0), pos(Xn, Yn)), pos(X, Y)) :-
    X >= X0, X < Xn,
    Y >= Y0, Y < Yn.

moore_neighbor_dirs = neighbor_dirs ++ [
                          vec(-1, -1),
                          vec( 1, -1),
                          vec(-1,  1),
                          vec( 1,  1)
                      ].

moore_neighbors(P) = map(plus(P), moore_neighbor_dirs).

neighbor_dirs = [ vec( 0, -1),
                  vec(-1,  0),
                  vec( 1,  0),
                  vec( 0,  1) ].

neighbors(P) = map(plus(P), neighbor_dirs).
