package day16

type BitStream struct {
	bytes []byte
	nBits int // Bits left of the first byte.
}

func NewBitStream(bytes []byte) *BitStream {
	return &BitStream{bytes, 8}
}

func (s *BitStream) Size() int {
	return 8*len(s.bytes) + s.nBits - 8
}

func (s *BitStream) Get(n int) uint64 {
	if n < 1 || n > 64 {
		panic("invalid bit size")
	}

	var bits uint64

	for n > 0 {
		var m = min(n, s.nBits)
		var k = s.nBits - m

		var byte uint8 = s.bytes[0] >> k
		var mask uint8 = ^(0xFF << m)

		bits = (bits << m) | uint64(byte&mask)

		n -= m
		s.nBits -= m
		if s.nBits == 0 {
			s.bytes = s.bytes[1:]
			s.nBits = 8
		}
	}

	return bits
}
