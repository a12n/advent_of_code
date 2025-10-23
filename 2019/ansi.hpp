#ifndef ANSI_HPP
#define ANSI_HPP

#include <ostream>

namespace ansi {

struct erase {
    enum {
        end = 0,
        begin = 1,
        entire = 2,
    } what
        = entire;
};

struct hide_cursor {
    bool hide = true;
};

struct cursor_position {
    size_t row = 0, col = 0;
};

std::ostream& operator<<(std::ostream& out, erase cmd);
std::ostream& operator<<(std::ostream& out, hide_cursor cmd);
std::ostream& operator<<(std::ostream& out, cursor_position cmd);

} // namespace ansi

#endif  // ANSI_HPP
