package day20

import (
	"bufio"
	"log"
	"strconv"

	"a12n/advent_of_code/2021/lib"
)

const (
	X = iota
	Y
)

type Image struct{ lib.MapGrid[struct{}] }
type Kernel [3][3]bool

func NewImage() Image {
	return Image{make(lib.MapGrid[struct{}])}
}

func ReadImage(r *bufio.Reader) (Image, error) {
	var err error
	var image = NewImage()

	if image.MapGrid, err = lib.ReadMapGrid(r, lib.Point2{},
		func(char byte) (*struct{}, error) {
			switch char {
			case '.':
				return nil, nil
			case '#':
				var pixel struct{}
				return &pixel, nil
			default:
				return nil, strconv.ErrSyntax
			}
		},
	); err != nil {
		return Image{}, err
	}

	return image, nil
}

func NewKernel(image Image, p lib.Point2) (k Kernel) {
	var off lib.Vector2
	for _, off[Y] = range [3]int{-1, 0, 1} {
		for _, off[X] = range [3]int{-1, 0, 1} {
			k[off[Y]+1][off[X]+1] = image.Has(p.Add(off))
		}
	}
	return
}

func (k Kernel) ToKey() (key int) {
	for i := 0; i < 9; i++ {
		if k[2-i/3][2-i%3] {
			key |= (1 << i)
		}
	}
	log.Printf("ToKey %v = %#09b %d", k, key, key)
	return
}

func (i Image) Enhance(outside bool, alg *Algorithm) (Image, bool) {
	var extent = i.Extent()
	var result = NewImage()
	var key = func(p lib.Point2) (ans int) {
		for k, off := range [9]lib.Vector2{
			0: lib.Vector2{1, 1},
			1: lib.Vector2{0, 1},
			2: lib.Vector2{-1, 1},
			3: lib.Vector2{1, 0},
			4: lib.Vector2{0, 0},
			5: lib.Vector2{-1, 0},
			6: lib.Vector2{1, -1},
			7: lib.Vector2{0, -1},
			8: lib.Vector2{-1, -1},
		} {
			var pixel bool
			var q = p.Add(off)
			if extent.Contains(q) {
				pixel = i.Has(q)
			} else {
				pixel = outside
			}
			if pixel {
				ans |= (1 << k)
			}
		}
		return
	}

	for p := range extent.Expand(1).AllPoints() {
		var k = key(p)
		if alg[k] {
			result.Set(p, struct{}{})
			log.Println("p", p, "k", k, "#")
		}
	}

	return result, alg[Kernel{
		{outside, outside, outside},
		{outside, outside, outside},
		{outside, outside, outside},
	}.ToKey()]
}
