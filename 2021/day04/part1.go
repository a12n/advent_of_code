package day04

import (
	"bufio"
	"fmt"
	"io"
	"strconv"
	"strings"
)

func Part1(r *bufio.Reader, w io.Writer) error {
	var board *Board
	var boards []*Board
	var err error
	var numbers []int

	if numbers, err = ReadNumbers(r); err != nil {
		return err
	}

	if boards, err = ReadBoards(r); err != nil {
		return err
	}

	var cell Cell
	var cells = make(CellMap)
	var n int

	for _, board = range boards {
		cells.Index(board)
	}

	for _, n = range numbers {
		for _, cell = range cells[n] {
			cell.Board.Mark(cell.Row, cell.Col)
			if cell.Board.Wins(cell.Row, cell.Col) {
				fmt.Fprintln(w, n*cell.Board.Score())
				return nil
			}
		}
	}

	return fmt.Errorf("no winning board")
}

func ReadNumbers(r *bufio.Reader) ([]int, error) {
	var err error
	var numbers []int
	var str string

	if str, err = r.ReadString('\n'); err != nil {
		return nil, err
	}

	for _, str = range strings.Split(strings.TrimSpace(str), ",") {
		var n int64

		if n, err = strconv.ParseInt(str, 10, 24); err != nil {
			return nil, err
		}

		numbers = append(numbers, int(n))
	}

	return numbers, nil
}
