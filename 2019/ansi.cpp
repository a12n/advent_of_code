#include "ansi.hpp"

namespace ansi {

constexpr const char csi = '\x1b';

std::ostream& operator<<(std::ostream& out, erase cmd)
{
    return out << csi << '[' << static_cast<int>(cmd.what) << 'J';
}

std::ostream& operator<<(std::ostream& out, hide_cursor cmd)
{
    return out << csi << "[?25" << (cmd.hide ? 'l' : 'h');
}

std::ostream& operator<<(std::ostream& out, cursor_position cmd)
{
    return out << csi << '[' << (cmd.row + 1) << ';' << (cmd.col + 1) << 'H';
}

} // namespace ansi
