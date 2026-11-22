#!/usr/bin/gawk -E

BEGIN {
  digit = 13
  print                                                                \
    "package day24" ORS                                                \
    ORS                                                                \
    "var inputs = [14]func(int, int) int{"
}

/inp/ {
  if (digit < 13) {
    print                                           \
      "		return z" ORS                           \
      "	},"
  }
  print                                         \
    "	" digit-- ": func(w, z int) int {" ORS  \
    "		var x, y int"
}

/add/ { print "		" $2 " += " $3 }
/mul/ { print "		" $2 " *= " $3 }
/div/ { print "		" $2 " /= " $3 }
/mod/ { print "		" $2 " %= " $3 }

/eql/ {
  print                                              \
    "		if " $2 " == " $3 " {" ORS                   \
    "			" $2 " = 1" ORS                          \
    "		} else {" ORS                                \
    "			" $2 " = 0" ORS                          \
    "		}"
}

END {
  print                                            \
    "		return z" ORS                          \
    "	}," ORS                                    \
    "}"
}
