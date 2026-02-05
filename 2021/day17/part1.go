package day17

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"math"
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

func Part1(r *bufio.Reader, w io.Writer) error {
	var err error
	var target, velocity struct {
		x, y struct {
			min int
			max int
		}
	}

	if _, err = fmt.Fscanf(r,
		"target area: x=%d..%d, y=%d..%d",
		&target.x.min, &target.x.max,
		&target.y.min, &target.y.max,
	); err != nil {
		return err
	}

	if target.x.min > target.x.max {
		target.x.min, target.x.max = target.x.max, target.x.min
	}
	if target.y.min > target.y.max {
		target.y.min, target.y.max = target.y.max, target.y.min
	}

	var roots [2]float64
	var vx, vy, h int

	roots, _ = quadratic(1, -2*float64(target.x.min))
	velocity.x.min = int(math.Ceil(roots[1]))
	log.Println("roots", roots)

	roots, _ = quadratic(1, -2*float64(target.x.max))
	velocity.x.max = int(math.Floor(roots[1]))
	log.Println("roots", roots)
	log.Println("velocity.x", velocity.x)

	for vx = velocity.x.min; vx <= velocity.x.max; vx++ {
		for vy = 1; vy < 1000; vy++ {
			var t [2]int

			log.Println("vx", vx, "vy", vy)

			roots, _ = quadratic(-2*float64(vy), 2*float64(target.y.max))
			t[0] = int(math.Ceil(roots[1]))
			log.Println("roots", roots)

			roots, _ = quadratic(-2*float64(vy), 2*float64(target.y.min))
			t[1] = int(math.Floor(roots[1]))
			log.Println("roots", roots)
			log.Println("t", t)

			if t[0] < vx || t[1] < vx || t[0] > t[1] {
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

func quadratic(p, q float64) ([2]float64, bool) {
	var d = (p*p)/(2*2) - q
	if d < 0 {
		return [2]float64{}, false
	}
	d = math.Sqrt(d)
	return [2]float64{-p/2 - d, -p/2 + d}, true
}
