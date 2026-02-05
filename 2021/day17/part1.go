package day17

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"math"

	"a12n/advent_of_code/2021/lib"
)

const (
	X = iota
	Y
)

// x(t) = x0 + v(t) t
//
// x = x0 + v t
// v = v0 + a t
//
// x = x0 + (v0 + 1/2 a t) t
// x = x0 + v0 t + 1/2 a t^2
// x0 = 0
//
// x = v0 t + 1/2 a t^2
// a = { -1|0, -1 }

// x = vx t + 1/2 ax t²
// y = vy t + 1/2 ay t²

// x = vx t + 1/2 ax t²
// y = vy t + 1/2 ay t²

// vx t + 1/2 ax t² ≥ min_x
// vx t + 1/2 ax t² ≤ max_x
// vy t + 1/2 ay t² ≥ min_y
// vy t + 1/2 ay t² ≤ max_y

// vx t + 1/2 ax t² ≥ min_x
// vx t + 1/2 ax t² ≤ max_x
// vy t + 1/2 -1 t² ≥ min_y
// vy t + 1/2 -1 t² ≤ max_y

// t - parameter
// vx, vy - variables
//
// vx t - 1/2 t² ≥ min_x
// vx t - 1/2 t² ≤ max_x
// XXX: vx + t ≥ t
// vy t - 1/2 t² ≥ min_y
// vy t - 1/2 t² ≤ max_y

// The highest Y position (at which velocity becomes zero, for non-negative initial velocity) at time t:
// vy = vy0 + ay t
// 0 = vy0 + ay t
// 0 = vy0 - t
// t = vy0
//
// The highest Y position (non-negative initial):
// y = vy0 t + 1/2 ay t²
// 2 y = 2 vy0 t - t²
// 2 y = 2 vy0 vy0 - vy0²
// 2 y = 2 vy0² - vy0²
// 2 y = vy0²
// 2 y = vy0²
//
// For integer velocity:
// y = vy0 + (vy0 - 1) + (vy0 - 2) + … + 1
// y = (vy0² + vy0) / 2
//
// Similar for time t to reach farthest X position:
// vx = vx0 + ax t
// 0 = vx0 + ax t
// 0 = vx0 - t
// t = vx0
//
// Farthest X position (at which velocity becomes zero) is again:
// x = (vx0² + vx0) / 2
//
// With the optimal trajectory (with the highest Y position in the
// middle) X velocity is zero in the target area. Otherwise it would
// be possible to shoot with less X velocity and more Y velocity and
// reach higher position. For the optimal initial velocity this
// wouldn't be possible.
//
// The optimal initial X velocity is an integer v such that:
// min_x ≤ (v² + v) / 2 ≤ max_x
// 2 min_x ≤ (v² + v) ≤ 2 max_x

// (v² + v) / 2 ≤ max_x
// (v² + v) / 2 ≥ min_x
// v² + v ≤ 2 max_x
// v² + v ≥ 2 min_x
// v² + v - 2 max_x ≤ 0
// v² + v - 2 min_x ≥ 0
//
// v² + p v + q = 0
// p = 1
// q = -2 max_x
// q = -2 min_x
//
// There are a few such values. The time t to reach that X position is
// equal to initial velocity vx0. So, time to consider for optimal Y
// velocity would be t ≥ vx0.
//
// vy t - 1/2 t² ≥ min_y
// vy t - 1/2 t² ≤ max_y
// t ≥ vx0
//
// -1/2 t² + vy t - min_y ≥ 0
// -1/2 t² + vy t - max_y ≤ 0
// t ≥ vx0
//
// t² - 2 vy t + 2 min_y ≥ 0
// t² - 2 vy t + 2 max_y ≤ 0
// t ≥ vx0

func quadratic(p, q float64) ([2]float64, bool) {
	var d = (p*p)/(2*2) - q
	if d < 0 {
		return [2]float64{}, false
	}
	d = math.Sqrt(d)
	return [2]float64{-p/2 - d, -p/2 + d}, true
}

func optimal(x [2]int, y [2]int) {
	roots0, _ := quadratic(1, -2*float64(x[0]))
	roots1, _ := quadratic(1, -2*float64(x[1]))
	fromX := int(math.Ceil(roots0[1]))
	toX := int(math.Floor(roots1[1]))
	log.Println("roots0", roots0, "roots1", roots1)

	for vx := fromX; vx <= toX; vx++ {
		log.Println("vx", vx)
		for vy := 1; vy < 1000; vy++ {
			roots0, _ = quadratic(-2*float64(vy), 2*float64(y[0]))
			roots1, _ = quadratic(-2*float64(vy), 2*float64(y[1]))
			log.Println("vy", vy, "roots0", roots0, "roots1", roots1)
			fromT := int(math.Ceil(roots1[1]))
			toT := int(math.Floor(roots0[1]))
			if fromT >= vx && toT >= vx && fromT <= toT {
				log.Println("fromT", fromT, "toT", toT, "h", (vy*vy+vy)/2)
			}
		}
	}
}

// func reachesTarget2(vx int, tx [2]int) bool {
//
// }

func reachesTarget(v int, t [2]int, a func(int) int) bool {
	var p = 0
	for {
		log.Println("p", p, "v", v)
		if p >= t[0] && p <= t[1] {
			return true
		} else if p > t[1] {
			return false
		}
		var p2 = p + v
		var v2 = v + a(v)
		if p2 == p {
			return false
		}
		p = p2
		v = v2
	}
}

func xRange(target [2]int) lib.Set[int] {
	var ans = lib.Set[int]{}
	for vx := 0; vx <= target[1]; vx++ {
		if reachesTarget(vx, target, func(v int) int {
			if v > 0 {
				return -1
			} else if v < 0 {
				return +1
			} else {
				return 0
			}
		}) {
			ans.Add(vx)
		}
	}
	return ans
}

func yRange(target [2]int) lib.Set[int] {
	var ans = lib.Set[int]{}
	for vy := 0; vy < 1000; vy++ {
		if reachesTarget(-vy, target, func(int) int { return +1 }) {
			log.Println("add", -vy)
			ans.Add(-vy)
		}
	}
	return ans
}

func Part1(r *bufio.Reader, w io.Writer) error {
	var err error
	var x [2]int
	var y [2]int

	if _, err = fmt.Fscanf(r,
		"target area: x=%d..%d, y=%d..%d",
		&x[0], &x[1],
		&y[0], &y[1],
	); err != nil {
		return err
	}

	if x[0] > x[1] {
		x[0], x[1] = x[1], x[0]
	}
	if y[0] > y[1] {
		y[0], y[1] = y[1], y[0]
	}

	log.Println(x, y)
	optimal(x, y)

	return err
}
