package day20

import (
	"bufio"
	"strconv"
	"strings"
)

type Algorithm [512]bool

func ReadAlgorithm(r *bufio.Reader) (*Algorithm, error) {
	var alg Algorithm
	var err error
	var i int
	var line string

	if line, err = r.ReadString('\n'); err != nil {
		return nil, err
	}

	line = strings.TrimSpace(line)

	if len(line) != len(alg) {
		return nil, strconv.ErrRange
	}

	for i = range line {
		alg[i] = (line[i] == '#')
	}

	return &alg, nil
}
