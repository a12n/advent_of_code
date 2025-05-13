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

main(!IO) :- main(pos(0, 0), _, pos(10, 1), _, !IO).

:- pred main(pos::in, pos::out, pos::in, pos::out, io::di, io::uo) is det.
main(!ShipPos, !WaypointPos, !IO) :-
    read_line_as_string(ReadResult, !IO),
    ( ReadResult = ok(Line),
      ( command_string(Command, chomp(Line)) ->
        %% TODO
        main(!ShipPos, !WaypointPos, !IO)
      ; error("Invalid input")
      )
    ; ReadResult = eof,
      write_int(taxicab_dist(!.ShipPos), !IO), nl(!IO)
    ; ReadResult = error(Error),
      error(error_message(Error))
    ).
