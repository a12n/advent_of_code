%% -*- mode: mercury -*-
:- module day_02_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module int, string.
:- import_module day_02, string_ext.

main(!IO) :-
    write_num_valid(0, check(valid), !IO).

:- pred valid(policy::in, password::in) is semidet.
valid(policy(I, J, Letter), password(Password)) :-
    %% FIXME
    ( index(Password, I - 1, Letter), not index(Password, J - 1, Letter)
    ; not index(Password, I - 1, Letter), index(Password, J - 1, Letter)
    ).
