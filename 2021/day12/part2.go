package day12

import (
	"bufio"
	"fmt"
	"io"
	"log"
)

func Part2(r *bufio.Reader, w io.Writer) error {
	var caves Caves
	var err error

	if caves, err = ReadCaves(r); err != nil {
		return err
	}

	fmt.Fprintln(w, NewSearch(caves, "end").TwicePaths("start").NumPaths())

	return err

}

func (s *Search) TwicePaths(start string) *Search {
	for s.twice, _ = range s.caves {
		if s.twice == start || s.twice == s.finish || !IsSmall(s.twice) {
			continue
		}
		log.Println("twice", s.twice)
		s.OncePaths(start)
	}
	return s
}
