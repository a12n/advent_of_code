package day04

import (
	"bufio"
	"fmt"
	"io"
)

func Part2(r *bufio.Reader, w io.Writer) error {
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

	var ok bool
	var playing = make(map[*Board]struct{})
	var score = -1

	for _, board = range boards {
		cells.Index(board)
		playing[board] = struct{}{}
	}

	for _, n = range numbers {
		for _, cell = range cells[n] {
			if _, ok = playing[cell.Board]; !ok {
				continue
			}

			cell.Board.Mark(cell.Row, cell.Col)

			if cell.Board.Wins(cell.Row, cell.Col) {
				delete(playing, cell.Board)
				score = n * cell.Board.Score()
			}
		}
	}

	if score == -1 {
		return fmt.Errorf("no winning board")
	}

	fmt.Fprintln(w, score)

	return nil
}
