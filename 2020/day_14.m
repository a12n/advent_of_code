%% -*- mode: mercury -*-
:- module day_14.
:- interface.

:- import_module map.
:- import_module pair.
:- import_module uint. % XXX: Must be 64 bit.

:- type value == uint.
:- type address == value.
:- type mask == pair(value, value).
:- type instr ---> mask(mask); store(address, value).
:- type memory == map(address, value).

:- pred instr_string(string, instr).
:- mode instr_string(in, out) is semidet.

:- pred mask_string(string, mask).
:- mode mask_string(in, out) is semidet.

:- pred value_string(string, value).
:- mode value_string(in, out) is semidet.

:- implementation.

:- import_module list.
:- import_module string.

instr_string(String, Instr) :-
    [KeyStr, ValueStr] = split_at_char('=', String),
    ( "mask" = strip(KeyStr), mask_string(ValueStr, Mask) ->
      Instr = mask(Mask)
    ; append("mem[", MemKeyStr, strip(KeyStr)),
      append(AddrStr, "]", MemKeyStr),
      value_string(AddrStr, Addr),
      value_string(ValueStr, Value) ->
      Instr = store(Addr, Value)
    ; fail
    ).

mask_string(String, pair(Or, And)) :-
    base_string_to_uint(2, strip(replace_all(String, "X", "0")), Or),
    base_string_to_uint(2, strip(replace_all(replace_all(String, "1", "0"), "X", "1")), And).

value_string(String, Value) :-
    base_string_to_uint(10, strip(String), Value).
