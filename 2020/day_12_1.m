%% -*- mode: mercury -*-
:- module day_12_1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module require.
:- import_module string.

:- import_module day_12.
:- import_module grid.
:- import_module grid.plane.

main(!IO) :- main(pos(0, 0), _, '→', _, !IO).

:- pred main(pos::in, pos::out, dir::in, dir::out, io::di, io::uo) is det.
main(!ShipPos, !ShipDir, !IO) :-
    read_line_as_string(ReadResult, !IO),
    ( ReadResult = ok(Line),
      ( command_string(Command, chomp(Line)) ->
        navigation(Command, !ShipPos, !ShipDir),
        main(!ShipPos, !ShipDir, !IO)
      ; error("Invalid input")
      )
    ; ReadResult = eof,
      write_int(taxicab_dist(!.ShipPos), !IO), nl(!IO)
    ; ReadResult = error(Error),
      error(error_message(Error))
    ).

:- pred navigation(command::in, pos::in, pos::out, dir::in, dir::out) is det.
navigation(abs_move(Dir, N), !Pos, !Dir) :- !:Pos = plus(!.Pos, times(to_vec(coerce(Dir)), N)).
navigation(rotate(K), !Pos, !Dir)        :- !:Dir = rotate_dir(K, !.Dir).
navigation(rel_move(N), !Pos, !Dir)      :- navigation(abs_move(!.Dir, N), !Pos, !Dir).
