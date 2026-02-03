package day08

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"slices"
	"strings"
)

func SortedString(str string) string {
	var bytes = []byte(str)
	slices.Sort(bytes)
	return string(bytes)
}

func Map1478(patterns [10]string) (map[string]int, error) {
	var found [10]bool
	var result = make(map[string]int)
	var str string

	for _, str = range patterns {
		var n int

		switch len(str) {
		case 2: // 1
			n = 1
		case 3: // 7
			n = 7
		case 4: // 4
			n = 4
		case 7: // 8
			n = 8
		default:
			continue
		}

		if found[n] {
			return nil, fmt.Errorf("duplicate patterns for %d", n)
		}

		found[n] = true
		result[SortedString(str)] = n
	}

	return result, nil
}

func Part1(r *bufio.Reader, w io.Writer) error {
	var err error
	var i, num int

	for {
		var output [4]string
		var patterns [10]string

		if patterns, output, err = ReadEntry(r); err != nil {
			if err == io.EOF {
				break
			}
			return err
		}

		var digits map[string]int

		if digits, err = Map1478(patterns); err != nil {
			return err
		}

		log.Println("patterns", patterns, "output", output, "digits", digits)

		for i = range output {
			switch digits[output[i]] {
			case 1, 4, 7, 8:
				num++
			}
		}
	}

	fmt.Fprintln(w, num)

	return nil
}

func ParseEntry(str string) (patterns [10]string, output [4]string, err error) {
	var fields []string
	var i int

	if fields = strings.Split(str, " | "); len(fields) != 2 {
		err = fmt.Errorf("bad entry %v", fields)
		return
	}

	for i, str = range strings.Split(fields[0], " ") {
		if i >= len(patterns) {
			err = fmt.Errorf("bad patterns %q", fields[0])
			return
		}
		patterns[i] = SortedString(str)
	}

	for i, str = range strings.Split(fields[1], " ") {
		if i >= len(output) {
			err = fmt.Errorf("bad output %q", fields[1])
			return
		}
		output[i] = SortedString(str)
	}

	return
}

func ReadEntry(r *bufio.Reader) ([10]string, [4]string, error) {
	var err error
	var line string

	if line, err = r.ReadString('\n'); err != nil {
		return [10]string{}, [4]string{}, err
	}

	return ParseEntry(strings.TrimSpace(line))
}
