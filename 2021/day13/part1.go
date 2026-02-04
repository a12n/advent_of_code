package day13

import (
	"bufio"
	"fmt"
	"io"
	"log"
)

func Part1(r *bufio.Reader, w io.Writer) error {
	var err error
	var folds []Fold
	var paper Paper

	if paper, err = ReadPaper(r); err != nil {
		return err
	}

	if folds, err = ReadFolds(r); err != nil {
		return err
	}

	log.Println("paper", paper, "folds", folds)

	paper.Fold(folds[0])

	fmt.Fprintln(w, len(paper.MapGrid))

	return nil
}
