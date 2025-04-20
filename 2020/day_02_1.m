%% -*- mode: mercury -*-
:- module day_02_1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module int.
:- import_module day_02, string_ext.

main(!IO) :-
    write_num_valid(0, check(valid), !IO).

:- pred valid(policy::in, password::in) is semidet.
valid(policy(MinTimes, MaxTimes, Letter), password(Password)) :-
    contains_char(Password, Letter, Times),
    Times >= MinTimes,
    Times =< MaxTimes.
