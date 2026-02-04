package day10

var Closing = map[byte]byte{'(': ')', '<': '>', '[': ']', '{': '}'}

func IsValid(s string) (bool, int, []byte) {
	var i int
	var opened []byte

	for i = range s {
		switch s[i] {
		case '(', '<', '[', '{':
			opened = append(opened, s[i])
		case ')', '>', ']', '}':
			if s[i] != Closing[opened[len(opened)-1]] {
				// Corrupted.
				return false, i, opened
			}
			opened = opened[:len(opened)-1]
		}
	}

	if len(opened) != 0 {
		// Incomplete.
		return false, len(s), opened
	}

	// OK.
	return true, 0, nil
}

func SyntaxScore(mismatch byte) int {
	var score = map[byte]int{')': 3, ']': 57, '}': 1197, '>': 25137}
	return score[mismatch]
}

func AutocompleteScore(opened []byte) int {
	var score = map[byte]int{')': 1, ']': 2, '}': 3, '>': 4}
	var total int

	for len(opened) > 0 {
		total = 5*total + score[Closing[opened[len(opened)-1]]]
		opened = opened[:len(opened)-1]
	}

	return total
}
