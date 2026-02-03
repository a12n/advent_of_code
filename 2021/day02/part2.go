package day02

import (
	"bufio"
	"fmt"
	"io"
)

func Part2(r *bufio.Reader, w io.Writer) error {
	var aim, depth, position int

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
			aim -= cmd.Arg
		case Down:
			aim += cmd.Arg
		case Forward:
			depth += aim * cmd.Arg
			position += cmd.Arg
		}
	}

	fmt.Fprintln(w, depth*position)

	return nil
}
