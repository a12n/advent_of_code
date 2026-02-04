package lib

type Set[Elt comparable] map[Elt]struct{}

func (s Set[Elt]) Add(x Elt) {
	s[x] = struct{}{}
}

func (s Set[Elt]) Delete(x Elt) {
	delete(s, x)
}

func (s Set[Elt]) IsElement(x Elt) (ok bool) {
	_, ok = s[x]
	return
}
