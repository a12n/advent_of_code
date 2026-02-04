package day15

import (
	"bufio"
	"container/heap"
	"fmt"
	"io"

	"a12n/advent_of_code/2021/lib"
)

func Part1(r *bufio.Reader, w io.Writer) error {
	var err error
	var grid lib.Grid[int]

	if grid, err = lib.ReadDigitGrid(r); err != nil {
		return err
	}

	var extent = grid.Extent()

	fmt.Fprintln(w, LowestTotalRisk(grid, extent, extent.Begin, extent.MaxPoint()))

	return err
}

func LowestTotalRisk(grid lib.Grid[int], extent lib.Extent2, start, finish lib.Point2) int {
	var parent = make(map[lib.Point2]lib.Point2)
	var queue Queue

	heap.Push(&queue, State{0, start})
	parent[start] = start

	for queue.Len() > 0 {
		var dir lib.Dir
		var u = heap.Pop(&queue).(State)

		if u.Pos == finish {
			return u.Risk
		}

		for _, dir = range lib.AllDir {
			var adj = u.Pos.Add(dir.ToVector())
			var visited bool

			if !extent.Contains(adj) {
				continue
			}

			if _, visited = parent[adj]; visited {
				continue
			}

			heap.Push(&queue, State{Risk: u.Risk + grid.Get(adj), Pos: adj})
			parent[adj] = u.Pos
		}

	}

	return 0
}

type State struct {
	Risk int
	Pos  lib.Point2
}

type Queue []State

func (q Queue) Len() int {
	return len(q)
}

func (q Queue) Less(i, j int) bool {
	return q[i].Risk < q[j].Risk
}

func (q Queue) Swap(i, j int) {
	q[i], q[j] = q[j], q[i]
}

func (q *Queue) Push(x any) {
	*q = append(*q, x.(State))
}

func (q *Queue) Pop() any {
	var n = len(*q)
	var elt = (*q)[n-1]
	*q = (*q)[:n-1]
	return elt
}
