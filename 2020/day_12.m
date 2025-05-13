%% -*- mode: mercury -*-
:- module day_12.
:- interface.

:- import_module grid.
:- import_module grid.plane.

:- type command ---> abs_move(dir, int); rotate(int); rel_move(int).

:- pred command_string(command, string).
:- mode command_string(out, in) is semidet.

:- pred basic_navigation(command::in, pos::in, pos::out, dir::in, dir::out) is det.

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

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

basic_navigation(abs_move(Dir, N), !Pos, !Dir) :- !:Pos = plus(!.Pos, times(to_vec(coerce(Dir)), N)).
basic_navigation(rotate(K), !Pos, !Dir)        :- !:Dir = rotate_dir(K, !.Dir).
basic_navigation(rel_move(N), !Pos, !Dir)      :- basic_navigation(abs_move(!.Dir, N), !Pos, !Dir).
