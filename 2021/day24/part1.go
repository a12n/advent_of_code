package day24

import (
	"bufio"
	"fmt"
	"io"
	"strconv"
	"strings"
)

type Program []Instruction

type Instruction struct {
	Op   OpCode
	Dest Reg
	Src  struct {
		Reg
		int
	}
}

type OpCode int
type Reg int

const (
	Inp OpCode = iota
	Add
	Mul
	Div
	Mod
	Eql
)

const (
	W Reg = iota
	X
	Y
	Z
)

type ALU [4]int

func ParseOpCode(s string) (OpCode, error) {
	switch strings.ToLower(s) {
	case "inp":
		return Inp, nil
	case "add":
		return Add, nil
	case "mul":
		return Mul, nil
	case "div":
		return Div, nil
	case "mod":
		return Mod, nil
	case "eql":
		return Eql, nil
	default:
		return 0, fmt.Errorf("opcode %q: %w", s, strconv.ErrSyntax)
	}
}

func ParseReg(s string) (Reg, error) {
	switch strings.ToLower(s) {
	case "w":
		return W, nil
	case "x":
		return X, nil
	case "y":
		return Y, nil
	case "z":
		return Z, nil
	default:
		return 0, fmt.Errorf("reg %q: %w", s, strconv.ErrSyntax)
	}
}

func ReadProgram(r *bufio.Reader) (Program, error) {
	var prog Program

	for {
		var err error
		var instr Instruction
		var line string

		if line, err = r.ReadString('\n'); err != nil {
			if err == io.EOF {
				break
			}
			return nil, err
		}

		if line = strings.TrimSpace(line); line == "" {
			continue
		}

		if instr, err = ParseInstruction(line); err != nil {
			return nil, err
		}

		prog = append(prog, instr)
	}

	return prog, nil
}

func ParseInstruction(s string) (Instruction, error) {
	var fields = strings.Fields(strings.TrimSpace(s))
	var werr = func(err error) error { return fmt.Errorf("instruction %q: %w", s, err) }

	if len(fields) < 2 {
		return Instruction{}, werr(strconv.ErrSyntax)
	}

	var err error
	var instr Instruction

	if instr.Op, err = ParseOpCode(fields[0]); err != nil {
		return Instruction{}, werr(err)
	}

	if instr.Dest, err = ParseReg(fields[1]); err != nil {
		return Instruction{}, werr(err)
	}

	if instr.Op == Inp && len(fields) == 2 {
		return instr, nil
	}

	if len(fields) != 3 {
		return Instruction{}, werr(strconv.ErrSyntax)
	}

	if instr.Src.Reg, err = ParseReg(fields[2]); err != nil {
		if instr.Src.int, err = strconv.Atoi(fields[2]); err != nil {
			return Instruction{}, werr(strconv.ErrSyntax)
		}
		instr.Src.Reg = Reg(-1)
	}

	return instr, nil
}

func Part1(r *bufio.Reader, w io.Writer) error {
	var err error
	var prog Program

	if prog, err = ReadProgram(r); err != nil {
		return err
	}

	fmt.Println(prog)

	return nil
}
