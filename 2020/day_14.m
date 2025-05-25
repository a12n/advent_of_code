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

:- func init_mask = mask.

:- pred instr_string(string, instr).
:- mode instr_string(in, out) is semidet.

%%---------------------------------------------------------------------------
%% v1 decoder chip

:- type memory == map(address, value).

:- func init_memory = memory.
:- pred decoder_chip(instr::in, mask::in, mask::out, memory::in, memory::out) is det.

:- implementation.

:- import_module list.
:- import_module string.

init_mask = pair(0u, 0xF_FFFF_FFFFu).

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

:- pred mask_string(string, mask).
:- mode mask_string(in, out) is semidet.
mask_string(String, pair(OrMask, AndMask)) :-
    base_string_to_uint(2, strip(replace_all(String, "X", "0")), OrMask),
    base_string_to_uint(2, strip(replace_all(replace_all(String, "1", "0"), "X", "1")), AndMask).

:- pred value_string(string, value).
:- mode value_string(in, out) is semidet.
value_string(String, Value) :-
    base_string_to_uint(10, strip(String), Value).

init_memory = init.

decoder_chip(Instr, (OrMask - AndMask) @ !.Mask, !:Mask, !Memory) :-
    ( Instr = mask(!:Mask)
    ; Instr = store(Address, Value),
      set(Address, OrMask \/ (AndMask /\ Value), !Memory)
    ).
