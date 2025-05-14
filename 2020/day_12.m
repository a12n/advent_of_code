%% -*- mode: mercury -*-
:- module day_12.
:- interface.

:- import_module grid.
:- import_module grid.plane.

:- type command ---> abs_move(dir, int); rotate(int); rel_move(int).

:- pred command_string(command, string).
:- mode command_string(out, in) is semidet.

:- implementation.

:- import_module char.
:- import_module int.
:- import_module string.

command_string(Command, String) :-
    first_char(String, Char, ArgStr), to_int(ArgStr, Arg),
    ( Char = 'N', Command = abs_move('↑', Arg)
    ; Char = 'S', Command = abs_move('↓', Arg)
    ; Char = 'E', Command = abs_move('→', Arg)
    ; Char = 'W', Command = abs_move('←', Arg)
    ; Char = 'L', angle_turns(Arg, N), Command = rotate(-N)
    ; Char = 'R', angle_turns(Arg, N), Command = rotate(N)
    ; Char = 'F', Command = rel_move(Arg)
    ).

:- pred angle_turns(int::in, int::out) is semidet.
angle_turns( 90, 1).
angle_turns(180, 2).
angle_turns(270, 3).
