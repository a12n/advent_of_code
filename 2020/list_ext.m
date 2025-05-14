%% -*- mode: mercury -*-
:- module list_ext.
:- interface.

:- import_module list.

:- func reduce((func(T, T) = T), list(T)) = T is semidet.

:- implementation.

reduce(F, [H | T]) = foldl(F, T, H).
