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

:- pred instr_string(string::in, instr::out) is semidet.

%%---------------------------------------------------------------------------
%% v1 decoder chip

:- type memory == map(address, value).

:- func init_memory = memory.
:- func memory_sum(memory) = value.

:- pred decoder_chip(instr::in, mask::in, mask::out, memory::in, memory::out) is det.

%%---------------------------------------------------------------------------
%% v2 decoder chip

:- pred decoder_chip_floating(instr::in, mask::in, mask::out, memory::in, memory::out) is det.

%%---------------------------------------------------------------------------
:- implementation.

:- import_module list.
:- import_module solutions.
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

:- pred mask_string(string::in, mask::out) is semidet.
mask_string(String, pair(OrMask, AndMask)) :-
    base_string_to_uint(2, strip(replace_all(String, "X", "0")), OrMask),
    base_string_to_uint(2, strip(replace_all(replace_all(String, "1", "0"), "X", "1")), AndMask).

:- pred value_string(string::in, value::out) is semidet.
value_string(String, Value) :-
    base_string_to_uint(10, strip(String), Value).

init_memory = init.

memory_sum(Memory) = Sum :-
    foldl_values(plus, Memory, 0u, Sum).

decoder_chip(Instr, (OrMask - AndMask) @ !.Mask, !:Mask, !Memory) :-
    ( Instr = mask(!:Mask)
    ; Instr = store(Address, Value),
      set(Address, OrMask \/ (AndMask /\ Value), !Memory)
    ).

decoder_chip_floating(Instr, (OrMask - AndMask) @ !.Mask, !:Mask, !Memory) :-
    ( Instr = mask(!:Mask)
    ; Instr = store(Address, Value),
      solutions(floating_address(Address \/ OrMask, AndMask), Addresses),
      foldl(set_value(Value), Addresses, !Memory)
    ).

:- pred floating_address(address::in, address::in, address::out) is multi.
floating_address(Address, AndMask, Result) :-
    ( AndMask = 0u -> Result = Address
    ; floating_address(Address >> 1, AndMask >> 1, ShiftedResult),
      ( AndMask /\ 1u = 1u,
        ( Result = (ShiftedResult << 1) \/ 1u
        ; Result = (ShiftedResult << 1) \/ 0u
        )
      ; Result = (ShiftedResult << 1) \/ (Address /\ 1u)
      )
    ).

%% map.foldl_values/4 requires predicate, but uint.plus/2 is a
%% function.
:- pred plus(uint, uint, uint).
:- mode plus(in, in, out) is det.
plus(A, B, plus(A, B)).

%% Like map.set/4, but with key and value in different order.
:- pred set_value(V::in, K::in, map(K, V)::in, map(K, V)::out) is det.
set_value(V, K, !Map) :- set(K, V, !Map).
