package day10

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"strings"
)

func Part1(r *bufio.Reader, w io.Writer) error {
	var err error
	var total int

	for {
		var i int
		var line string
		var ok bool

		if line, err = r.ReadString('\n'); err != nil {
			if err == io.EOF {
				break
			}
			return err
		}

		line = strings.TrimSpace(line)

		if ok, i, _ = IsValid(line); !ok && i < len(line) {
			log.Printf("%q corrupted, @%d '%c'", line, i, line[i])
			total += SyntaxScore(line[i])
		}
	}

	fmt.Fprintln(w, total)

	return nil
}
