package day02

import (
	"bufio"
	"fmt"
	"io"
	"strings"
)

type Direction int

const (
	Up Direction = iota + 1
	Down
	Forward
)

func ParseDirection(str string) (Direction, error) {
	switch strings.ToLower(strings.TrimSpace(str)) {
	case "up":
		return Up, nil
	case "down":
		return Down, nil
	case "forward":
		return Forward, nil
	}
	return 0, fmt.Errorf("bad direction %q", str)
}

type Command struct {
	Dir Direction
	Arg int
}

func ReadCommand(r *bufio.Reader) (Command, error) {
	var cmd Command
	var err error
	var str string

	if _, err = fmt.Fscanln(r, &str, &cmd.Arg); err != nil {
		return Command{}, err
	}

	if cmd.Dir, err = ParseDirection(str); err != nil {
		return Command{}, err
	}

	return cmd, nil
}

func ReadCommands(r *bufio.Reader) ([]Command, error) {
	var cmds []Command
	var err error

	for {
		var cmd Command
		var s string

		if _, err = fmt.Fscanln(r, &s, &cmd.Arg); err != nil {
			if err == io.EOF {
				return cmds, nil
			}
			return nil, err
		}

		if cmd.Dir, err = ParseDirection(s); err != nil {
			return nil, err
		}

		cmds = append(cmds, cmd)
	}
}
