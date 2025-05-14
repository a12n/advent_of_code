%% -*- mode: mercury -*-
:- module day_09.
:- interface.

:- import_module array.
:- import_module list.

:- pred encryption_weakness(int::in, list(int)::in, int::out) is nondet.
:- pred invalid_number(array(int)::array_ui, int::in, int::out) is nondet.

:- implementation.

:- import_module int.
:- import_module string.

:- import_module array_ext.
:- import_module list_ext.

invalid_number(Numbers, Preamble, N) :- invalid_number(Numbers, 0, Preamble, N).

:- pred invalid_number(array(int)::array_ui, int::in, int::in, int::out) is nondet.
invalid_number(Numbers, Begin, End, N) :-
    ( semidet_lookup(Numbers, End, N),
      not (
          member(Numbers, Begin, End, A),
          member(Numbers, Begin, End, B),
          N = A + B
      )
    ; invalid_number(Numbers, Begin + 1, End + 1, N)
    ).

encryption_weakness(Invalid, Numbers, Weakness) :- encryption_weakness(Invalid, Numbers, [], Weakness).

:- pred encryption_weakness(int::in, list(int)::in, list(int)::in, int::out) is nondet.
encryption_weakness(Invalid, [N | Numbers], Sums, Weakness) :-
    trace [io(!IO)] io.format(io.stderr_stream, "Invalid %s, Numbers %s, Sums %s\n", [s(string(Invalid)), s(string([N|Numbers])), s(string(Sums))], !IO),
    take_while((pred(Elt::in) is semidet :- Elt \= Invalid), Sums, Part, Found),
    trace [io(!IO)] io.format(io.stderr_stream, "Sums %s, Part %s, Found %s\n", [s(string(Sums)), s(string(Part)), s(string(Found))], !IO),
    ( Found = [Invalid | _] ->
      %% Tmp = [Invalid | reverse([0 | Part])],
      %% difference(Tmp, Elts),
      Elts = map_corresponding(minus, Part ++ [Invalid], [0 | Part]),
      Min = reduce(min, Elts),
      Max = reduce(max, Elts),
      trace [io(!IO)] io.format(io.stderr_stream, "Elts %s, Min %s, Max %s\n", [s(string(Elts)), s(string(Min)), s(string(Max))], !IO),
      Weakness = Min + Max
    ; encryption_weakness(Invalid, Numbers, map(plus(N), [0 | Sums]), Weakness)
    ).


    %% ( nth_member_search(Sums, Invalid, I), index1(Sums, I - 1, RangeLast), index1(Sums, 1, RangeFirst) ->
    %%   %% FIXME: Not first and last, but min and max in the range.
    %%   trace [io(!IO)] io.format(io.stderr_stream, "I %d, RangeLast %d, RangeFirst %d, %d + %d\n", [i(I), i(RangeLast), i(RangeFirst), i(RangeFirst), i(Invalid - RangeLast)], !IO),
    %%   Weakness = RangeFirst + (Invalid - RangeLast)
    %% ; encryption_weakness(Invalid, Numbers, map(plus(N), [0 | Sums]), Weakness)
    %% ).
