package day10

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"slices"
	"strings"
)

func Part2(r *bufio.Reader, w io.Writer) error {
	var err error
	var scores []int

	for {
		var i int
		var line string
		var ok bool
		var opened []byte

		if line, err = r.ReadString('\n'); err != nil {
			if err == io.EOF {
				break
			}
			return err
		}

		line = strings.TrimSpace(line)

		if ok, i, opened = IsValid(line); !ok && i == len(line) {
			log.Printf("%q incomplete, opened %q", line, string(opened))
			scores = append(scores, AutocompleteScore(opened))
		}
	}

	slices.Sort(scores)
	log.Println("scores", len(scores), scores)
	fmt.Fprintln(w, scores[len(scores)/2])

	return nil
}
