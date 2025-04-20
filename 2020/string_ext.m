%% -*- mode: mercury -*-
:- module string_ext.
:- interface.

:- import_module char, int, string.

:- pred contains_char(string, char, int).
:- mode contains_char(in, in, out) is det.
%% :- mode contains_char(in, out, in) is nondet.

:- implementation.

contains_char(String, Char, Times) :-
    Times = foldl((func(C, N) = (C = Char -> N + 1; N)), String, 0).
