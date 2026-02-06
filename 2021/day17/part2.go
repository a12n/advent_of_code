package day17

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"math"
)

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
	vyMax = 1000 // Arbitrary

	log.Println("vxMin", vxMin, "vxMax", vxMax)
	log.Println("vyMin", vyMin, "vyMax", vyMax)

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
			// At time t=vx projectile X speed will be zero. Any time
			// t>vx must be treated as t=vx.
			for t = tMin; t <= min(tMax, vx); t++ {
				var x = (2*vx*t - t*t) / 2

				log.Println("v", [2]int{vx, vy}, "t", t, "x", x)

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
