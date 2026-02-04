package day13

import (
	"bufio"
	"io"
	"log"
	// "os"
)

func Part2(r *bufio.Reader, w io.Writer) error {
	var err error
	var fold Fold
	var folds []Fold
	var paper Paper

	if paper, err = ReadPaper(r); err != nil {
		return err
	}

	if folds, err = ReadFolds(r); err != nil {
		return err
	}

	log.Println("paper", paper, "folds", folds)

	for _, fold = range folds {
		// paper.Print(os.Stderr, paper.MaxDot())
		paper.Fold(fold)
	}

	if err = paper.Print(w, paper.Extent()); err != nil {
		return err
	}

	return nil
}
