%% -*- mode: mercury -*-
:- module day_11.
:- interface.

:- import_module array2d.
:- import_module io.
:- import_module maybe.

:- type seat ---> empty; occupied.
:- type seat_layout == array2d(maybe(seat)).

:- pred input_seat_layout(res(seat_layout)::out, io::di, io::uo) is det.
:- pred simulate(seat_layout::in, seat_layout::array2d_uo) is det.

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.

:- import_module array2d_ext.
:- import_module grid.
:- import_module grid.plane.
:- import_module io_ext.

:- pred seat_char(char, maybe(seat)).
:- mode seat_char(in, out) is semidet.
%% :- mode seat_char(out, in) is det.
seat_char('.', no).
seat_char('L', yes(empty)).
seat_char('#', yes(occupied)).

input_seat_layout(Result, !IO) :-
    input_chars_lines(ReadResult, !IO),
    ( ReadResult = ok(Lines),
      ( map(map(seat_char), Lines, SeatsLists) ->
        Result = ok(from_lists(SeatsLists))
      ; Result = error(make_io_error("Invalid input"))
      )
    ; ReadResult = error(Error),
      Result = error(Error)
    ).

simulate(!Seats) :- simulate(bounds(!.Seats), !Seats).

:- pred simulate(extent::in, seat_layout::in, seat_layout::out) is det.
simulate(Extent, !Seats) :-
    %% XXX: Update in-place.
    map_foldl(
        (pred(Row::in, Col::in, !.Elt::in, !:Elt::out, !.Changed::in, !:Changed::out) is det :-
             NeighborPos = filter(in_bounds(Extent), moore_neighbors(pos(Col, Row))),
             Num = length(filter(unify(yes(occupied)), map(lookup(!.Seats), NeighborPos))),
             ( !.Elt = yes(empty),    Num = 0 -> !:Elt = yes(occupied), !:Changed = yes
             ; !.Elt = yes(occupied), Num > 3 -> !:Elt = yes(empty),    !:Changed = yes
             ; true
             )
        ), !Seats, no, Changed
    ),
    ( Changed = yes -> simulate(Extent, !Seats); true ).

:- func lookup(seat_layout, pos) = maybe(seat).
lookup(Array, pos(Col, Row)) = array2d.lookup(Array, Row, Col).

:- func bounds(seat_layout) = extent.
bounds(Array) = extent(pos(0, 0), pos(NumCols, NumRows)) :-
    array2d.bounds(Array, NumRows, NumCols).
