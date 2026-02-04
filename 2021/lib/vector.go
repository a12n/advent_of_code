package lib

type Vector2 [2]int

func (u Vector2) Mul(n int) Vector2 {
	return Vector2{u[0] * n, u[1] * n}
}

func (u Vector2) TaxicabNorm() int {
	return Abs(u[0]) + Abs(u[1])
}

type Vector3 [3]int
