%% -*- mode: mercury -*-
:- module day_09.
:- interface.

:- import_module array.

:- pred weakness_number(array(int)::array_ui, int::in, int::out) is nondet.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

:- import_module array_ext.

weakness_number(Numbers, Preamble, N) :- weakness_number(Numbers, 0, Preamble, N).

:- pred weakness_number(array(int)::array_ui, int::in, int::in, int::out) is nondet.
weakness_number(Numbers, Begin, End, N) :-
    ( semidet_lookup(Numbers, End, N),
      not (
          member(Numbers, Begin, End, A),
          member(Numbers, Begin, End, B),
          N = A + B
      )
    ; weakness_number(Numbers, Begin + 1, End + 1, N)
    ).
