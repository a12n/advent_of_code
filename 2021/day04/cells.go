package day04

type Cell struct {
	Board    *Board
	Row, Col int
}

type CellMap map[int][]Cell

func (m CellMap) Index(board *Board) {
	var row, col int
	for row = 0; row < Size; row++ {
		for col = 0; col < Size; col++ {
			m[board[row][col]] = append(m[board[row][col]], Cell{board, row, col})
		}
	}
}
