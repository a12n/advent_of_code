package lib

import "fmt"

type Axis int

const (
	X = iota
	Y
	Z
)

func (a *Axis) Scan(state fmt.ScanState, _ rune) error {
	var char rune
	var err error

	if char, _, err = state.ReadRune(); err != nil {
		return err
	}

	switch char {
	case 'x', 'X':
		*a = X
	case 'y', 'Y':
		*a = Y
	case 'z', 'Z':
		*a = Z
	default:
		return fmt.Errorf("bad axis '%c'", char)
	}

	return nil
}

func (a Axis) String() string {
	return [3]string{"X", "Y", "Z"}[a]
}
