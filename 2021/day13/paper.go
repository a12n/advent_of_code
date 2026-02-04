package day13

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"strings"

	"a12n/advent_of_code/2021/lib"
)

type Paper struct {
	lib.MapGrid[struct{}]
}

func ReadPaper(r *bufio.Reader) (Paper, error) {
	var err error
	var paper = Paper{make(lib.MapGrid[struct{}])}

	for {
		var line string
		var p lib.Point2

		if line, err = r.ReadString('\n'); err != nil {
			if err == io.EOF {
				break
			}
			return Paper{}, err
		}

		line = strings.TrimSpace(line)

		if line == "" {
			break
		}

		if _, err = fmt.Sscanf(line, "%d,%d", &p[X], &p[Y]); err != nil {
			return Paper{}, err
		}

		paper.Set(p, struct{}{})
	}

	return paper, nil
}

func (l Paper) Fold(fold Fold) {
	var p lib.Point2
	for p, _ = range l.MapGrid {
		if p[fold.Along] > fold.int {
			var q = p
			q[fold.Along] = fold.int - (q[fold.Along] - fold.int)
			log.Println("fold", fold, p, "->", q)
			delete(l.MapGrid, p)
			l.MapGrid.Set(q, struct{}{})
		}
	}
}

func (l Paper) Print(w io.Writer, extent lib.Extent2) error {
	var printer lib.GridPrinter[struct{}]
	return printer.Print(w, l, extent, nil)
}
