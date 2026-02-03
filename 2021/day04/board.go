package day04

import (
	"bufio"
	"fmt"
	"io"
)

const (
	Marked = -1
	Size   = 5
)

type Board [Size][Size]int

func ReadBoard(r *bufio.Reader) (*Board, error) {
	var board Board
	var err error

	if _, err = fmt.Fscan(r,
		&board[0][0], &board[0][1], &board[0][2], &board[0][3], &board[0][4],
		&board[1][0], &board[1][1], &board[1][2], &board[1][3], &board[1][4],
		&board[2][0], &board[2][1], &board[2][2], &board[2][3], &board[2][4],
		&board[3][0], &board[3][1], &board[3][2], &board[3][3], &board[3][4],
		&board[4][0], &board[4][1], &board[4][2], &board[4][3], &board[4][4],
	); err != nil {
		return nil, err
	}

	return &board, nil
}

func ReadBoards(r *bufio.Reader) ([]*Board, error) {
	var board *Board
	var boards []*Board
	var err error

	for {
		if board, err = ReadBoard(r); err != nil {
			if err == io.EOF {
				return boards, nil
			}
			return nil, err
		}
		boards = append(boards, board)
	}
}

func (b *Board) Mark(row, col int) {
	b[row][col] = Marked
}

func (b *Board) WinsRow(row int) bool {
	return b[row][0] == Marked &&
		b[row][1] == Marked &&
		b[row][2] == Marked &&
		b[row][3] == Marked &&
		b[row][4] == Marked
}

func (b *Board) WinsCol(col int) bool {
	return b[0][col] == Marked &&
		b[1][col] == Marked &&
		b[2][col] == Marked &&
		b[3][col] == Marked &&
		b[4][col] == Marked
}

func (b *Board) Wins(row, col int) bool {
	return b.WinsRow(row) || b.WinsCol(col)
}

func (b *Board) Score() int {
	var row, col, sum int
	for row = 0; row < Size; row++ {
		for col = 0; col < Size; col++ {
			if b[row][col] != Marked {
				sum += b[row][col]
			}
		}
	}
	return sum
}
