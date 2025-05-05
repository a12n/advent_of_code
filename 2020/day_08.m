%% -*- mode: mercury -*-
:- module day_08.
:- interface.

:- import_module array, io.

:- type opcode ---> acc; jmp; nop.
:- type instruction ---> {opcode, int}.
:- type program == array(instruction).

:- pred opcode_string(string, opcode).
:- mode opcode_string(in, out) is semidet.
:- mode opcode_string(out, in) is det.

:- pred instruction_string(string::in, instruction::out) is semidet.

:- pred program_input(res::out, program::array_uo, io::di, io::uo) is det.

:- pred instruction_run(instruction::in, int::in, int::out, int::in, int::out) is det.

:- implementation.

:- import_module int, list, string.

opcode_string("acc", acc).
opcode_string("jmp", jmp).
opcode_string("nop", nop).

instruction_string(String, {Opcode, Arg}) :-
    [OpcodeString, ArgString] = words(String),
    opcode_string(OpcodeString, Opcode),
    to_int(ArgString, Arg).

program_input(Result, Program, !IO) :-
    program_input(Result, 0, init(1000, {nop, 0}), Program, !IO).

:- pred program_input(res::out, int::in, program::array_di, program::array_uo, io::di, io::uo) is det.
program_input(Result, IP, !Program, !IO) :-
    read_line_as_string(ReadResult, !IO),
    ( ReadResult = ok(Line),
      ( instruction_string(chomp(Line), Instruction) ->
        set(IP, Instruction, !.Program, !:Program),
        program_input(Result, IP + 1, !Program, !IO)
      ; Result = error(make_io_error("Invalid instruction"))
      )
    ; ReadResult = eof,
      resize(IP, {nop, 0}, !.Program, !:Program),
      Result = ok
    ; ReadResult = error(Error),
      Result = error(Error)
    ).

instruction_run({acc, Arg}, !Acc, !IP) :- !:Acc = !.Acc + Arg, !:IP = !.IP + 1.
instruction_run({jmp, Arg}, !Acc, !IP) :- !:Acc = !.Acc, !:IP = !.IP + Arg.
instruction_run({nop, _}, !Acc, !IP) :- !:Acc = !.Acc, !:IP = !.IP + 1.
