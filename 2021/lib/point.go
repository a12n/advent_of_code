package lib

type Point2 [2]int

func (p Point2) Add(u Vector2) Point2 {
	return Point2{
		p[0] + u[0],
		p[1] + u[1],
	}
}

func (p Point2) SubV(u Vector2) Point2 {
	return Point2{
		p[0] - u[0],
		p[1] - u[1],
	}
}

func (p Point2) SubP(q Point2) Vector2 {
	return Vector2(p.SubV(Vector2(q)))
}

type Point3 [3]int

func (p Point3) Add(u Vector3) Point3 {
	return Point3{
		p[0] + u[0],
		p[1] + u[1],
		p[2] + u[2],
	}
}

func (p Point3) SubV(u Vector3) Point3 {
	return Point3{
		p[0] - u[0],
		p[1] - u[1],
		p[2] - u[2],
	}
}

func (p Point3) SubP(q Point3) Vector3 {
	return Vector3(p.SubV(Vector3(q)))
}

func (p Point3) DistanceSqr(q Point3) int {
	return p.SubP(q).NormSqr()
}
