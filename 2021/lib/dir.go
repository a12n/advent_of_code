package lib

type Dir int

const (
	Up Dir = iota
	Down
	Left
	Right
)

var AllDir = []Dir{Up, Down, Left, Right}

func (d Dir) Neg() Dir {
	return [4]Dir{
		Up:    Down,
		Down:  Up,
		Left:  Right,
		Right: Left,
	}[d]
}

func (d Dir) ToVector() Vector2 {
	return [4]Vector2{
		Up:    Vector2{Y: -1},
		Down:  Vector2{Y: +1},
		Left:  Vector2{X: -1},
		Right: Vector2{X: +1},
	}[d]
}
