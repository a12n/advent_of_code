#ifndef GRID_HPP
#define GRID_HPP

#include <array>
#include <cstdint>
#include <istream>
#include <ostream>

//----------------------------------------------------------------------------
// 2D Grids

namespace grid::planar {

enum class direction {
    up,
    left,
    right,
    down,
};

enum class rotation {
    cw,
    ccw,
};

struct offset : public std::array<int64_t, 2> { };
struct position : public std::array<int64_t, 2> { };

direction opposite(direction dir);
int64_t taxicab_norm(offset u);
direction to_direction(char c);
position to_position(offset u);
offset to_offset(direction dir);
offset to_offset(position p);
offset normalize(offset u);
direction rotate(rotation rdir, direction dir);
double angle(offset u);

offset& operator*=(offset& u, int64_t n);
offset operator*(int64_t n, offset u);
offset& operator/=(offset& u, int64_t n);
offset operator/(offset u, int64_t n);
position& operator+=(position& p, offset u);
position operator+(position p, offset u);
offset& operator+=(offset& u, offset v);
offset operator+(offset u, offset v);
offset operator-(position p, position q);
position& operator-=(position& p, offset u);
position operator-(position p, offset u);
offset operator-(offset u);

std::istream& operator>>(std::istream& in, direction& dir);

std::ostream& operator<<(std::ostream& out, offset u);
std::ostream& operator<<(std::ostream& out, position p);

} // namespace grid::planar

template <>
struct std::hash<grid::planar::position> {
    size_t operator()(grid::planar::position p) const noexcept
    {
        return std::hash<int64_t> {}(p[0]) ^ (std::hash<int64_t> {}(p[1]) << 1);
    }
};

template <>
struct std::hash<grid::planar::offset> {
    size_t operator()(grid::planar::offset u) const noexcept
    {
        return std::hash<grid::planar::position> {}(to_position(u));
    }
};

//----------------------------------------------------------------------------
// 3D Grids

namespace grid::spatial {

struct offset : public std::array<int64_t, 3> { };
struct position : public std::array<int64_t, 3> { };

int64_t taxicab_norm(offset u);
offset to_offset(position p);
position to_position(offset u);

offset& operator*=(offset& u, int64_t n);
offset operator*(int64_t n, offset u);
offset& operator/=(offset& u, int64_t n);
offset operator/(offset u, int64_t n);
position& operator+=(position& p, offset u);
position operator+(position p, offset u);
offset& operator+=(offset& u, offset v);
offset operator+(offset u, offset v);
offset operator-(position p, position q);
position& operator-=(position& p, offset u);
position operator-(position p, offset u);
offset operator-(offset u);

std::ostream& operator<<(std::ostream& out, offset u);
std::ostream& operator<<(std::ostream& out, position p);

} // namespace grid::spatial

#endif // GRID_HPP
