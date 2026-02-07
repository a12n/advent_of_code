package day17

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"math"

	"a12n/advent_of_code/2021/lib"
)

// x(t) = x0 + v(t) t
//
// p = p0 + v t
// v = v0 + a t
//
// p = p0 + (v0 + 1/2 a t) t
// p = p0 + v0 t + 1/2 a t^2
// p0 = 0
//
// p = v0 t + 1/2 a t^2
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

// The highest Y position (at which velocity becomes zero, for
// non-negative initial velocity) at time t:
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
// But for y it's actually like this (eq1):
// yH = (vy0² + vy0) / 2 — to the highest point
// y0 = -((v0² + vy0) / 2) — back to the zero
// yN = -(vy0 + 1 + vy0 + 2 + vy0 + 3 + vy0 + 4 + … + vy0 + N) = -vy0 * (1 + 2 + 3 + 4 + … + N) = -vy0 * ((N * (N + 1))/2)
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

const (
	X = iota
	Y
)

type Position struct {
	Min, Max lib.Point2
}

func Part1(r *bufio.Reader, w io.Writer) error {
	var err error
	var target Position

	if target, err = ReadInput(r); err != nil {
		return err
	}

	var roots [2]float64
	var vxMin, vxMax int
	var vx, vy, h int

	roots, _ = Quadratic(1, -2*float64(target.Min[X]))
	vxMin = int(math.Ceil(roots[1]))
	log.Println("roots", roots)

	roots, _ = Quadratic(1, -2*float64(target.Max[X]))
	vxMax = int(math.Floor(roots[1]))
	log.Println("roots", roots)
	log.Println("vxMin", vxMin, "vxMax", vxMax)

	for vx = vxMin; vx <= vxMax; vx++ {
		for vy = 1; vy < 1000; vy++ {
			var tMin, tMax int

			log.Println("vx", vx, "vy", vy)

			roots, _ = Quadratic(-2*float64(vy), 2*float64(target.Max[Y]))
			tMin = int(math.Ceil(roots[1]))
			log.Println("roots", roots)

			roots, _ = Quadratic(-2*float64(vy), 2*float64(target.Min[Y]))
			tMax = int(math.Floor(roots[1]))
			log.Println("roots", roots)
			log.Println("tMin", tMin, "tMax", tMax)

			if tMin < vx || tMax < vx || tMin > tMax {
				// Out of target area
				continue
			}

			h = (vy * (vy + 1)) / 2
			log.Println("h", h)
		}
	}

	fmt.Fprintln(w, h)

	return err
}

func Part2(r *bufio.Reader, w io.Writer) error {
	var err error
	var target Position

	if target, err = ReadInput(r); err != nil {
		return err
	}

	log.Println("target", target)

	var roots [2]float64
	var vxMin, vxMax int
	var vyMin, vyMax int
	var vx, vy, t, n int

	// At vxMin initial X speed projectile speed will be zero at
	// the beginning of the target area. With initial X speed less
	// than vxMin it would be impossible to reach target area.
	roots, _ = Quadratic(1, -2*float64(target.Min[0]))
	vxMin = int(math.Ceil(roots[1]))

	// At initial X speed greater than vxMax the projectile will
	// overshoot the target area just at T+1.
	vxMax = target.Max[X]

	vyMin = target.Min[Y]
	vyMax = -vyMin // Due to (eq1)

	log.Println("vxMin", vxMin, "vxMax", vxMax)
	log.Println("vyMin", vyMin, "vyMax", vyMax)
	log.Println("n", (vxMax-vxMin)*(vyMax-vyMin))

	for vy = vyMin; vy <= vyMax; vy++ {
		var tMin, tMax int

		log.Println("vy", vy)

		// The target are is always at negative Y, while the
		// shooting starts at zero. So, the projectile will first
		// cross the target.Max[Y] and then later will cross the
		// target.Min[Y] (both negative).
		roots, _ = Quadratic(-2*float64(vy), 2*float64(target.Max[Y]))
		tMin = int(math.Ceil(roots[1]))
		log.Println("roots", roots)
		log.Println("tMin", tMin)

		roots, _ = Quadratic(-2*float64(vy), 2*float64(target.Min[Y]))
		tMax = int(math.Floor(roots[1]))
		log.Println("roots", roots)
		log.Println("tMax", tMax)

		if tMin > tMax {
			log.Println("infeasible with vy", vy)
			continue
		}

		for vx = vxMin; vx <= vxMax; vx++ {
			log.Println("vx", vx)

			// Will X be in the target area at time from tMin to tMax?
			for t = tMin; t <= tMax; t++ {
				// At time t=vx projectile X speed will be zero. Any time
				// t>vx must be treated as t=vx.
				var t2 = min(tMax, vx)
				var x = (2*vx*t2 - t2*t2) / 2

				log.Println("v", [2]int{vx, vy}, "t", t, "t2", t2, "x", x)

				if x < target.Min[X] || x > target.Max[X] {
					log.Println("infeasible with vx", vx)
					continue
				}

				log.Println("feasible with v", [2]int{vx, vy})
				n++
			}
		}
	}

	fmt.Fprintln(w, n)

	return err
}

func ReadInput(r *bufio.Reader) (Position, error) {
	var err error
	var p Position

	if _, err = fmt.Fscanf(r,
		"target area: x=%d..%d, y=%d..%d",
		&p.Min[0], &p.Max[0],
		&p.Min[1], &p.Max[1],
	); err != nil {
		return Position{}, err
	}

	if p.Min[0] > p.Max[0] {
		p.Min[0], p.Max[0] = p.Max[0], p.Min[0]
	}
	if p.Min[1] > p.Max[1] {
		p.Min[1], p.Max[1] = p.Max[1], p.Min[1]
	}

	return p, nil
}

func Quadratic(p, q float64) ([2]float64, bool) {
	var d = (p*p)/(2*2) - q
	if d < 0 {
		return [2]float64{}, false
	}
	d = math.Sqrt(d)
	return [2]float64{-p/2 - d, -p/2 + d}, true
}
