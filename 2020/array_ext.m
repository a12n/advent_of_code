%% -*- mode: mercury -*-
:- module array_ext.
:- interface.

:- import_module array.

%% Members of the array at indices [Begin, End).
:- pred member(array(T)::in, int::in, int::in, T::out) is nondet.

:- implementation.

:- import_module int.

member(Array, Begin, End, Elt) :-
    Begin < End,
    ( semidet_lookup(Array, Begin, Elt)
    ; member(Array, Begin + 1, End, Elt)
    ).
