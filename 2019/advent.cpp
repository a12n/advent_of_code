#include "advent.hpp"

namespace grid::planar {

direction opposite(direction dir)
{
    switch (dir) {
    case direction::up:
        return direction::down;
    case direction::left:
        return direction::right;
    case direction::right:
        return direction::left;
    case direction::down:
        return direction::up;
    default:
        throw std::invalid_argument(__func__);
    }
}

int64_t taxicab_norm(offset u)
{
    return std::abs(u[0]) + std::abs(u[1]);
}

direction to_direction(char c)
{
    switch (c) {
    case 'U':
    case 'u':
        return direction::up;
    case 'L':
    case 'l':
        return direction::left;
    case 'R':
    case 'r':
        return direction::right;
    case 'D':
    case 'd':
        return direction::down;
    default:
        throw std::invalid_argument(__func__);
    }
}

position to_position(offset u)
{
    return { u[0], u[1] };
}

offset to_offset(direction d)
{
    switch (d) {
    case direction::up:
        return { 0, 1 };
    case direction::left:
        return { -1, 0 };
    case direction::right:
        return { 1, 0 };
    case direction::down:
        return { 0, -1 };
    default:
        throw std::invalid_argument(__func__);
    }
}

offset to_offset(position p)
{
    return { p[0], p[1] };
}

offset& operator*=(offset& u, int64_t n)
{
    u[0] *= n;
    u[1] *= n;
    return u;
}

offset operator*(int64_t n, offset u)
{
    return u *= n;
}

position& operator+=(position& p, offset u)
{
    p[0] += u[0];
    p[1] += u[1];
    return p;
}

position operator+(position p, offset u)
{
    return p += u;
}

offset& operator+=(offset& u, offset v)
{
    u[0] += v[0];
    u[1] += v[1];
    return u;
}

offset operator+(offset u, offset v)
{
    return u += v;
}

offset operator-(position p, position q)
{
    return { p[0] - q[0], p[1] - q[1] };
}

position& operator-=(position& p, offset u)
{
    p[0] -= u[0];
    p[1] -= u[1];
    return p;
}

position operator-(position p, offset u)
{
    return p -= u;
}

offset operator-(offset v)
{
    return { -v[0], -v[1] };
}

std::istream& operator>>(std::istream& in, direction& dir)
{
    char c;
    if (in >> c) {
        try {
            dir = to_direction(c);
        } catch (std::invalid_argument&) {
            in.setstate(std::ios::failbit);
        }
    }
    return in;
}

std::ostream& operator<<(std::ostream& out, offset u)
{
    return out << '[' << u[0] << ',' << u[1] << ']';
}

std::ostream& operator<<(std::ostream& out, position p)
{
    return out << to_offset(p);
}

} // namespace grid::planar

namespace intcode {

std::tuple<opcode, mode, mode, mode> decode(value v)
{
    return {
        opcode(v % 100),
        mode((v / 100) % 10),
        mode((v / 1000) % 10),
        mode((v / 10000) % 10)
    };
}

address run(memory& m, address ip)
{
    while (true) {
        const auto [op, mode1, mode2, mode3] = decode(m[ip]);
        switch (op) {
        case opcode::add:
            m[m[ip + 3]] = m[m[ip + 1]] + m[m[ip + 2]];
            ip += 4;
            break;
        case opcode::mul:
            m[m[ip + 3]] = m[m[ip + 1]] * m[m[ip + 2]];
            ip += 4;
            break;
        case opcode::halt:
            return ip;
        default:
            throw std::invalid_argument(__func__);
        }
    }
}

std::istream& operator>>(std::istream& in, memory& m)
{
    return ::operator>><int64_t, ','>(in, m);
}

} // namespace intcode
