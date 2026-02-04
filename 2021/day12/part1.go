package day12

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"strings"
)

func Part1(r *bufio.Reader, w io.Writer) error {
	var caves Caves
	var err error

	if caves, err = ReadCaves(r); err != nil {
		return err
	}

	log.Println("caves", caves)

	fmt.Fprintln(w, NewSearch(caves, "end").OncePaths("start").NumPaths())

	return err
}

type Caves map[string][]string

func IsSmall(cave string) bool {
	return cave[0] >= 'a' && cave[0] <= 'z'
}

func ReadCaves(r *bufio.Reader) (Caves, error) {
	var caves = make(Caves)
	var err error

	for {
		var fields []string
		var line string

		if line, err = r.ReadString('\n'); err != nil {
			if err == io.EOF {
				break
			}
			return nil, err
		}

		fields = strings.Split(strings.TrimSpace(line), "-")
		caves[fields[0]] = append(caves[fields[0]], fields[1])
		caves[fields[1]] = append(caves[fields[1]], fields[0])
	}

	return caves, nil
}

type Search struct {
	caves  Caves
	finish string

	visited map[string]int
	twice   string

	path  []string
	paths map[string]struct{}
}

func NewSearch(caves Caves, finish string) *Search {
	return &Search{
		caves:   caves,
		finish:  finish,
		visited: make(map[string]int),
		paths:   make(map[string]struct{}),
	}
}

func (s *Search) OncePaths(start string) *Search {
	s.path = append(s.path, start)
	defer func() {
		s.path = s.path[:len(s.path)-1]
	}()

	if start == s.finish {
		// Found a path.
		s.paths[strings.Join(s.path, ",")] = struct{}{}
		return s
	}

	var next string

	if IsSmall(start) {
		var maxVisits = 1
		if start == s.twice {
			maxVisits = 2
		}
		if s.visited[start] == maxVisits {
			// Already visited.
			return s
		}
		s.visited[start]++
	}

	for _, next = range s.caves[start] {
		s.OncePaths(next)
	}

	if IsSmall(start) {
		s.visited[start]--
	}

	return s
}

func (s *Search) NumPaths() int {
	return len(s.paths)
}
