%% -*- mode: mercury -*-
:- module day_08.
:- interface.

:- import_module array, bool, io, pair.

:- type opcode ---> acc; jmp; nop.
:- type operand == int.
:- type instr == pair(opcode, operand).
:- type program == array(instr).

:- pred opcode_string(string, opcode).
:- mode opcode_string(in, out) is semidet.
:- mode opcode_string(out, in) is det.

:- pred instr_string(string, instr).
:- mode instr_string(in, out) is semidet.
%% :- mode instr_string(out, in) is det.

:- pred program_input(res::out, program::array_uo, io::di, io::uo) is det.

:- pred exec_instr(instr::in, int::in, int::out, int::in, int::out) is det.
:- pred exec_instr_nondet(instr::in, int::in, int::out, int::in, int::out) is multi.
:- pred exec_program(program::in, bool::out, int::in, int::out, int::in, int::out) is det.
:- pred repair_program(program::array_di, program::array_uo, int::in, int::out, int::in, int::out) is semidet.

:- implementation.

:- import_module int, list, ranges, string.

opcode_string("acc", acc).
opcode_string("jmp", jmp).
opcode_string("nop", nop).

instr_string(String, (Opcode - Arg)) :-
    [OpcodeStr, ArgStr] = words(String),
    opcode_string(OpcodeStr, Opcode),
    to_int(ArgStr, Arg).

program_input(Result, Program, !IO) :-
    program_input(Result, 0, init(1000, (nop - 0)), Program, !IO).

:- pred program_input(res::out, int::in, program::array_di, program::array_uo, io::di, io::uo) is det.
program_input(Result, IP, !Program, !IO) :-
    read_line_as_string(ReadResult, !IO),
    ( ReadResult = ok(Line),
      ( instr_string(chomp(Line), Instr) ->
        set(IP, Instr, !Program),
        program_input(Result, IP + 1, !Program, !IO)
      ; Result = error(make_io_error("Invalid instruction"))
      )
    ; ReadResult = eof,
      resize(IP, (nop - 0), !Program),
      Result = ok
    ; ReadResult = error(Error),
      Result = error(Error)
    ).

exec_instr((acc - Arg), !Acc, !IP) :- !:Acc = !.Acc + Arg, !:IP = !.IP + 1.
exec_instr((jmp - Arg), !Acc, !IP) :- !:IP = !.IP + Arg.
exec_instr((nop - _), !Acc, !IP) :- !:IP = !.IP + 1.

exec_instr_nondet((acc - Arg), !Acc, !IP) :- exec_instr((acc - Arg), !Acc, !IP).
exec_instr_nondet((jmp - Arg), !Acc, !IP) :- exec_instr((jmp - Arg), !Acc, !IP); exec_instr((nop - Arg), !Acc, !IP).
exec_instr_nondet((nop - Arg), !Acc, !IP) :- exec_instr((nop - Arg), !Acc, !IP); exec_instr((jmp - Arg), !Acc, !IP).

%% Like exec_instr_nondet/5, but only allows single instruction
%% replacement.
:- pred exec_instr_nondet(instr::in, bool::in, bool::out, int::in, int::out, int::in, int::out) is multi.
exec_instr_nondet((Opcode - Arg) @ Instr, !Det, !Acc, !IP) :-
    ( !.Det = yes, exec_instr(Instr, !Acc, !IP)
    ; Opcode = acc, exec_instr(Instr, !Acc, !IP)
    ; Opcode = jmp, ( exec_instr(Instr, !Acc, !IP)
                    ; exec_instr((nop - Arg), !Acc, !IP), !:Det = yes )
    ; Opcode = nop, ( exec_instr(Instr, !Acc, !IP)
                    ; exec_instr((jmp - Arg), !Acc, !IP), !:Det = yes )
    ).

exec_program(Program, Halts, !Acc, !IP) :-
    exec_program(Program, Halts, init(size(Program), no), _, !Acc, !IP).

:- pred exec_program(program::in, bool::out, array(bool)::array_di, array(bool)::array_uo, int::in, int::out, int::in, int::out) is det.
exec_program(Program, Halts, !Seen, !Acc, !IP) :-
    ( (!.IP < size(Program)) ->
      ( lookup(!.Seen, !.IP, no) ->
        lookup(Program, !.IP, Instr),
        set(!.IP, yes, !Seen),
        exec_instr(Instr, !Acc, !IP),
        exec_program(Program, Halts, !Seen, !Acc, !IP)
      ; Halts = no
      )
    ; Halts = yes
    ).

:- pred repair_opcode(opcode::in, opcode::out) is semidet.
repair_opcode(jmp, nop).
repair_opcode(nop, jmp).

repair_program(!Program, !Acc, !IP) :-
    ( (!.IP < size(!.Program)) ->
      lookup(!.Program, !.IP, (Opcode - Arg) @ Instr),
      ( repair_opcode(Opcode, OtherOpcode) ->
        set(!.IP, (OtherOpcode - Arg), !Program),
        ( exec_program(!.Program, yes, !Acc, !.IP, _) ->
          true
        ; set(!.IP, Instr, !Program),
          exec_instr(Instr, !Acc, !IP),
          repair_program(!Program, !Acc, !IP)
        )
      ; exec_instr(Instr, !Acc, !IP),
        repair_program(!Program, !Acc, !IP)
      )
    ; true
    ).
