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
main(!Pos, !Dir, !IO) :-
    read_line_as_string(ReadResult, !IO),
    ( ReadResult = ok(Line),
      ( command_string(Command, chomp(Line)) ->
        navigation(Command, !Pos, !Dir),
        main(!Pos, !Dir, !IO)
      ; error("Invalid input")
      )
    ; ReadResult = eof,
      write_int(taxicab_dist(!.Pos), !IO), nl(!IO)
    ; ReadResult = error(Error),
      error(error_message(Error))
    ).
