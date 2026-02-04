package day14

import (
	"bufio"
	"fmt"
	"io"
	"log"
)

func Part2(r *bufio.Reader, w io.Writer) error {
	var err error
	var polymer string
	var rules PairRules

	if _, err = fmt.Fscanf(r, "%s\n\n", &polymer); err != nil {
		return err
	}

	log.Printf("polymer %q", polymer)

	if rules, err = ReadPairRules(r); err != nil {
		return err
	}

	log.Println("rules", rules)

	var freqs = NewPairFreqs(polymer)

	log.Println("freqs", freqs)
	freqs = rules.Apply(freqs, 40)

	fmt.Fprintln(w, freqs.ElementFreqs(polymer[0]).Skew())

	return nil
}
