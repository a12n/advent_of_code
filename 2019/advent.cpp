#include "advent.hpp"

namespace grid {

int64_t taxicab_norm(vector u)
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

point to_point(vector u)
{
    return { u[0], u[1] };
}

vector to_vector(direction d)
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

vector to_vector(point p)
{
    return { p[0], p[1] };
}

vector operator*(int64_t n, vector u)
{
    return { n * u[0], n * u[1] };
}

point operator+(point p, vector u)
{
    return { p[0] + u[0], p[1] + u[1] };
}

vector operator+(vector u, vector v)
{
    return { u[0] + v[0], u[1] + v[1] };
}

vector operator-(point p, point q)
{
    return { p[0] - q[0], p[1] - q[1] };
}

point operator-(point p, vector u)
{
    return p + (-u);
}

vector operator-(vector v)
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

} // namespace grid

namespace intcode {

address run(memory& m, address ip)
{
    while (true) {
        switch (opcode(m[ip])) {
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
    return extract<int64_t>(in, m, ',');
}

} // namespace intcode
