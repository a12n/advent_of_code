package day01

import (
	"bufio"
	"fmt"
	"io"
	"math"
)

func Part1(r *bufio.Reader, w io.Writer) error {
	var depth = [2]int{0: math.MaxInt}
	var err error
	var num int

	for {
		if _, err = fmt.Fscan(r, &depth[1]); err != nil {
			if err == io.EOF {
				break
			}
			return err
		}
		if depth[1] > depth[0] {
			num++
		}
		depth[0] = depth[1]
	}

	fmt.Fprintln(w, num)

	return nil
}

func Part2(r *bufio.Reader, w io.Writer) error {
	var depth [4]int
	var err error
	var i, num int

	for ; ; i++ {
		if _, err = fmt.Fscan(r, &depth[i%len(depth)]); err != nil {
			if err == io.EOF {
				break
			}
			return err
		}

		if i > 2 {
			var smooth = [2]int{
				depth[(i-1)%len(depth)] + depth[(i-2)%len(depth)] + depth[(i-3)%len(depth)],
				depth[i%len(depth)] + depth[(i-1)%len(depth)] + depth[(i-2)%len(depth)],
			}
			if smooth[1] > smooth[0] {
				num++
			}
		}
	}

	fmt.Fprintln(w, num)

	return nil
}
