%% -*- mode: mercury -*-
:- module day_11.
:- interface.

:- import_module array2d.
:- import_module io.
:- import_module map.
:- import_module maybe.
:- import_module pair.

:- import_module grid.
:- import_module grid.plane.

:- type seat ---> empty; occupied.
:- type seat_layout == array2d(maybe(seat)).

:- pred input_seat_layout(res(seat_layout)::out, io::di, io::uo) is det.
:- pred simulate(seat_layout::in, seat_layout::array2d_uo) is det.

:- type sight_map == map(pair(pos, moore_dir), pos).
:- pred sight_map(seat_layout::in, sight_map::out) is det.

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.

:- import_module array2d_ext.
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

:- func num_occupied_neighbors(seat_layout, pos) = int.
num_occupied_neighbors(Seats, Pos) = N :-
    filter_map(semidet_lookup(Seats), moore_neighbors(Pos), Neighbors),
    foldl((pred(Seat::in, !.N::in, !:N::out) is det :-
               ( Seat = yes(occupied) -> !:N = !.N + 1; true )
          ), Neighbors, 0, N).

:- pred simulate(extent::in, seat_layout::in, seat_layout::out) is det.
simulate(Extent, !Seats) :-
    %% XXX: Update in-place.
    map_foldl(
        (pred(Row::in, Col::in, !.Elt::in, !:Elt::out, !.Changed::in, !:Changed::out) is det :-
             N = num_occupied_neighbors(!.Seats, to_pos(Row, Col)),
             ( !.Elt = yes(empty),    N = 0 -> !:Elt = yes(occupied), !:Changed = yes
             ; !.Elt = yes(occupied), N > 3 -> !:Elt = yes(empty),    !:Changed = yes
             ; true
             )
        ), !Seats, no, Changed
    ),
    ( Changed = yes -> simulate(Extent, !Seats); true ).

:- func lookup(seat_layout, pos) = maybe(seat).
lookup(Array, pos(Col, Row)) = array2d.lookup(Array, Row, Col).

:- pred semidet_lookup(seat_layout::in, pos::in, maybe(seat)::out) is semidet.
semidet_lookup(Array, pos(Col, Row), Elt) :-
    array2d_ext.semidet_lookup(Array, Row, Col, Elt).

:- func bounds(seat_layout) = extent.
bounds(Array) = extent(pos(0, 0), pos(NumCols, NumRows)) :-
    array2d.bounds(Array, NumRows, NumCols).

:- func to_pos(int, int) = pos.
to_pos(Row, Col) = pos(Col, Row).

:- pred sees(seat_layout::in, pos::in, moore_dir::in, pos::out) is semidet.
sees(Seats, FromPos, Dir, ToPos) :-
    AdjPos = adjacent(FromPos, Dir),
    semidet_lookup(Seats, AdjPos, Seat),
    ( Seat = yes(_), ToPos = AdjPos
    ; Seat = no, sees(Seats, AdjPos, Dir, ToPos)
    ).

sight_map(Seats, Mapping) :-
    foldl(
        (pred(FromPos::in, !.Map::in, !:Map::out) is det :-
             foldl(
                 (pred(Dir::in, !.Map::in, !:Map::out) is det :-
                      %% ( not contains(!.Map, (FromPos - Dir)), sees(Seats, FromPos, Dir, ToPos) ->
                      %%   set((FromPos - Dir), ToPos, !Map),
                      %%   set((ToPos - opposite(Dir)), FromPos, !Map)
                      %% ; true
                      %% )
                      ( sees(Seats, FromPos, Dir, ToPos) ->
                        set((FromPos - Dir), ToPos, !Map)
                      ; true
                      )
                 ),
                 moore_neighbor_dirs,
                 !Map
             )
        ),
        bounds(Seats), init, Mapping
    ).
