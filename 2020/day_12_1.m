%% -*- mode: mercury -*-
:- module day_12_1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

:- import_module grid.
:- import_module grid.plane.

:- type command ---> abs_move(dir, int); rotate(int); rel_move(int).

main(!IO) :- main(pos(0, 0), _, '→', _, !IO).

:- pred main(pos::in, pos::out, dir::in, dir::out, io::di, io::uo) is det.
main(!Pos, !Dir, !IO) :-
    read_line_as_string(ReadResult, !IO),
    ( ReadResult = ok(Line),
      ( command_string(Command, chomp(Line)) ->
        navigation(Command, !Pos, !Dir),
        trace [io(!IO)] format(stderr_stream, "%s: %s %s\n", [s(string(Command)), s(string(!.Pos)), s(string(!.Dir))], !IO),
        main(!Pos, !Dir, !IO)
      ; error("Invalid input")
      )
    ; ReadResult = eof,
      write_int(taxicab_dist(!.Pos), !IO), nl(!IO)
    ; ReadResult = error(Error),
      error(error_message(Error))
    ).

:- pred command_string(command, string).
:- mode command_string(out, in) is semidet.
command_string(Command, String) :-
    first_char(String, Char, ArgStr), to_int(ArgStr, Arg),
    ( Char = 'N', Command = abs_move('↑', Arg)
    ; Char = 'S', Command = abs_move('↓', Arg)
    ; Char = 'E', Command = abs_move('→', Arg)
    ; Char = 'W', Command = abs_move('←', Arg)
    ; Char = 'L', ( Arg = 90; Arg = 180; Arg = 270 ), Command = rotate(-(Arg / 90))
    ; Char = 'R', ( Arg = 90; Arg = 180; Arg = 270 ), Command = rotate(Arg / 90)
    ; Char = 'F', Command = rel_move(Arg)
    ).

:- pred navigation(command::in, pos::in, pos::out, dir::in, dir::out) is det.
navigation(abs_move(Dir, N), !Pos, !Dir) :- !:Pos = plus(!.Pos, times(to_vec(coerce(Dir)), N)).
navigation(rotate(K), !Pos, !Dir)        :- !:Dir = rotate_dir(K, !.Dir).
navigation(rel_move(N), !Pos, !Dir)      :- navigation(abs_move(!.Dir, N), !Pos, !Dir).
