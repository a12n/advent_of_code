package day02

import (
	"bufio"
	"fmt"
	"io"
)

func Part1(r *bufio.Reader, w io.Writer) error {
	var depth, position int

	for {
		var cmd Command
		var err error

		if cmd, err = ReadCommand(r); err != nil {
			if err == io.EOF {
				break
			}
			return err
		}

		switch cmd.Dir {
		case Up:
			depth -= cmd.Arg
		case Down:
			depth += cmd.Arg
		case Forward:
			position += cmd.Arg
		}
	}

	fmt.Fprintln(w, depth*position)

	return nil
}
