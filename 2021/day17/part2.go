package day17

import (
	"bufio"
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

	// At vxMin initial X speed projectile speed will be zero at
	// the beginning of the target area. With initial X speed less
	// than vxMin it would be impossible to reach target area.
	roots, _ = Quadratic(1, -2*float64(target.Min[0]))
	vxMin = int(math.Ceil(roots[1]))

	// At initial X speed greater than vxMax the projectile will
	// overshoot the target area just at T+1.
	vxMax = target.Max[X]

	log.Println("vxMin", vxMin, "vxMax", vxMax)

	return err
}
