%% -*- mode: mercury -*-
:- module day_12_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module require.
:- import_module string.

:- import_module day_12.
:- import_module grid.
:- import_module grid.plane.

main(!IO) :- main(pos(0, 0), _, vec(10, 1), _, !IO).

:- pred main(pos::in, pos::out, vec::in, vec::out, io::di, io::uo) is det.
main(!ShipPos, !WaypointVec, !IO) :-
    read_line_as_string(ReadResult, !IO),
    ( ReadResult = ok(Line),
      ( command_string(Command, chomp(Line)) ->
        navigation(Command, !ShipPos, !WaypointVec),
        main(!ShipPos, !WaypointVec, !IO)
      ; error("Invalid input")
      )
    ; ReadResult = eof,
      write_int(taxicab_dist(!.ShipPos), !IO), nl(!IO)
    ; ReadResult = error(Error),
      error(error_message(Error))
    ).

:- pred navigation(command::in, pos::in, pos::out, vec::in, vec::out) is det.
navigation(abs_move(Dir, N), !ShipPos, !WaypointVec) :-
    !:WaypointVec = plus_vec(!.WaypointVec, times(to_vec(coerce(Dir)), N)).
navigation(rotate(K), !ShipPos, !WaypointVec) :-
    !:WaypointVec = rotate_vec(K, !.WaypointVec).
navigation(rel_move(N), !ShipPos, !WaypointVec) :-
    !:ShipPos = plus(!.ShipPos, times(!.WaypointVec, N)).
