#include <cmath>
#include <numeric>
#include <stdexcept>

#include "grid.hpp"

namespace grid::planar {

std::string_view direction_symbol(direction dir, bool unicode)
{
    switch (dir) {
    case direction::up:
        return unicode ? "↑" : "^";
    case direction::left:
        return unicode ? "←" : "<";
    case direction::right:
        return unicode ? "→" : ">";
    case direction::down:
        return unicode ? "↓" : "v";
    default:
        throw std::invalid_argument(__func__);
    }
}

std::string_view rotation_symbol(rotation rdir, bool unicode)
{
    switch (rdir) {
    case rotation::cw:
        return unicode ? "↷" : "R";
    case rotation::ccw:
        return unicode ? "↶" : "L";
    default:
        throw std::invalid_argument(__func__);
    }
}

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
    case '^':
    case 'u':
        return direction::up;
    case '<':
    case 'L':
    case 'l':
        return direction::left;
    case '>':
    case 'R':
    case 'r':
        return direction::right;
    case 'D':
    case 'd':
    case 'v':
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
        return { 0, -1 };
    case direction::left:
        return { -1, 0 };
    case direction::right:
        return { 1, 0 };
    case direction::down:
        return { 0, 1 };
    default:
        throw std::invalid_argument(__func__);
    }
}

offset to_offset(position p)
{
    return { p[0], p[1] };
}

offset normalize(offset u)
{
    return u /= std::gcd(u[0], u[1]);
}

direction rotate(rotation rdir, direction dir)
{
    switch (rdir) {
    case rotation::cw:
        switch (dir) {
        case direction::up:
            return direction::right;
        case direction::left:
            return direction::up;
        case direction::right:
            return direction::down;
        case direction::down:
            return direction::left;
        };
        break;
    case rotation::ccw:
        switch (dir) {
        case direction::up:
            return direction::left;
        case direction::left:
            return direction::down;
        case direction::right:
            return direction::up;
        case direction::down:
            return direction::right;
        };
        break;
    }
    throw std::invalid_argument(__func__);
}

offset rotate(rotation rdir, offset u)
{
    // Implicit ±π/2 rotation matrix by vector multiplication.
    switch (rdir) {
    case rotation::cw:
        return { -u[1], u[0] };
    case rotation::ccw:
        return { u[1], -u[0] };
    }
    throw std::invalid_argument(__func__);
}

double angle(offset u)
{
    const auto theta = std::atan2(u[1], u[0]);
    return theta < 0 ? (theta + 2 * M_PI) : theta;
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

offset& operator/=(offset& u, int64_t n)
{
    u[0] /= n;
    u[1] /= n;
    return u;
}

offset operator/(offset u, int64_t n)
{
    return u /= n;
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

bool extent::insert(position p)
{
    bool inserted = false;
    for (size_t i = 0; i < 2; ++i) {
        if (p[i] < min_[i]) {
            min_[i] = p[i];
            inserted = true;
        }
        if (p[i] > max_[i]) {
            max_[i] = p[i];
            inserted = true;
        }
    }
    return inserted;
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

namespace grid::spatial {

int64_t taxicab_norm(offset u)
{
    return std::abs(u[0]) + std::abs(u[1]) + std::abs(u[2]);
}

offset to_offset(position p)
{
    return { p[0], p[1], p[2] };
}

position to_position(offset u)
{
    return { u[0], u[1], u[2] };
}

offset& operator*=(offset& u, int64_t n)
{
    u[0] *= n;
    u[1] *= n;
    u[2] *= n;
    return u;
}

offset operator*(int64_t n, offset u)
{
    return u *= n;
}

offset& operator/=(offset& u, int64_t n)
{
    u[0] /= n;
    u[1] /= n;
    u[2] /= n;
    return u;
}

offset operator/(offset u, int64_t n)
{
    return u /= n;
}

position& operator+=(position& p, offset u)
{
    p[0] += u[0];
    p[1] += u[1];
    p[2] += u[2];
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
    u[2] += v[2];
    return u;
}

offset operator+(offset u, offset v)
{
    return u += v;
}

offset operator-(position p, position q)
{
    return { p[0] - q[0], p[1] - q[1], p[2] - q[2] };
}

position& operator-=(position& p, offset u)
{
    p[0] -= u[0];
    p[1] -= u[1];
    p[2] -= u[2];
    return p;
}
position operator-(position p, offset u)
{
    return p -= u;
}
offset operator-(offset u)
{
    return { -u[0], -u[1], -u[2] };
}

std::ostream& operator<<(std::ostream& out, offset u)
{
    return out << '[' << u[0] << ',' << u[1] << ',' << u[2] << ']';
}

std::ostream& operator<<(std::ostream& out, position p)
{
    return out << to_offset(p);
}

} // namespace grid::spatial
