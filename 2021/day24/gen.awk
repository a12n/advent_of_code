#!/usr/bin/gawk -E

BEGIN {
  k = 14
  print                                                               \
    "package main" ORS                                                \
    ORS                                                               \
    "// import \"fmt\"" ORS                                           \
    "import \"log\"" ORS                                              \
    ORS                                                               \
    "func LargestModelNumber() int64 {" ORS                           \
    "	type link struct{" ORS                                        \
    "		accum int" ORS                                            \
    "		digit int" ORS                                            \
    "	}" ORS                                                        \
    ORS                                                               \
    "	var ans int64" ORS                                            \
    "	var w, x, y, z int" ORS                                       \
    "	var l link" ORS                                               \
    "	var carry" k " = map[int]link{0: l}"
}

function begin_input() {
  print                                                     \
    ORS                                                     \
    "	var carry" (k - 1) " = make(map[int]link)" ORS      \
    "	for l.digit = 1; l.digit <= 9; l.digit++ {" ORS     \
    "		for l.accum, _ = range carry" k " {" ORS        \
    "			w = l.digit" ORS                            \
    "			z = l.accum" ORS                            \
    "//			log.Println(" k ", \"z\", z, \"w\", w)"
}

function end_input() {
  print                                             \
    "			carry" k "[z] = l" ORS              \
    "//			log.Println(" k ", \"=\", z)" ORS   \
    "		}" ORS                                  \
    "	}"
  print \
    "	log.Println(" k ", \"len\", len(carry" k "), \"0\", carry" k "[0])"
}

/inp/ {
  if (k < 14) {
    end_input()
  }
  begin_input()
  k--
}

/add/ { print "" $2 " += " $3 }
/mul/ { print "" $2 " *= " $3 }
/div/ { print "" $2 " /= " $3 }
/mod/ { print "" $2 " %= " $3 }

/eql/ {
  print                                          \
    "if " $2 " == " $3 " {" ORS                  \
    "	" $2 " = 1" ORS                          \
    "} else {" ORS                               \
    "	" $2 " = 0" ORS                          \
    "}"
}

END {
  end_input()
  print                 \
    ORS                 \
    "	return ans" ORS \
    "}"
  print \
    ORS                                         \
    "func main() {" ORS                         \
    "	_ = LargestModelNumber()" ORS           \
    "}"
}
