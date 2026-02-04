package day13

import (
	"bufio"
	"fmt"
	"io"
	"strconv"
	"strings"

	"a12n/advent_of_code/2021/lib"
)

const (
	X = iota
	Y
	Z
)

type Fold struct {
	int
	Along lib.Axis
}

func ReadFolds(r *bufio.Reader) ([]Fold, error) {
	var err error
	var folds []Fold

	for {
		var fold Fold
		var line string

		if line, err = r.ReadString('\n'); err != nil {
			if err == io.EOF {
				break
			}
			return nil, err
		}

		line = strings.TrimSpace(line)

		if line == "" {
			break
		}

		if _, err = fmt.Sscanf(line, "fold along %v=%d", &fold.Along, &fold.int); err != nil {
			return nil, err
		}

		if fold.Along == Z {
			return nil, strconv.ErrRange
		}

		folds = append(folds, fold)
	}

	return folds, nil
}
