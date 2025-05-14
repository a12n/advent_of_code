%% -*- mode: mercury -*-
:- module advent.
:- interface.

%% Pipe operator.
:- func A >> (func(A) = B) = B.
:- mode in >> in(func(in) = out is det) = out is det.
:- mode in >> in(func(in) = out is semidet) = out is semidet.

:- implementation.

:- pragma inline('>>'/2).
A >> F = F(A).

:- func flip(func(A, B) = C) = (func(B, A) = C).
flip(F) = (func(B, A) = F(A, B)).
