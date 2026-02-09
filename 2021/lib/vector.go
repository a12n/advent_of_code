package lib

type Vector2 [2]int

func (u Vector2) Mul(n int) Vector2 {
	return Vector2{u[0] * n, u[1] * n}
}

func (u Vector2) TaxicabNorm() int {
	return Abs(u[0]) + Abs(u[1])
}

type Vector3 [3]int

func (u Vector3) Dot(v Vector3) int {
	return u[0]*v[0] + u[1]*v[1] + u[2]*v[2]
}

func (u Vector3) NormSqr() int {
	return u.Dot(u)
}
