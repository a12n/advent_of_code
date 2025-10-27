#ifndef GRID_HPP
#define GRID_HPP

#include <array>
#include <cstdint>
#include <istream>
#include <map>
#include <ostream>
#include <tuple>

//----------------------------------------------------------------------------
// 2D Grids

namespace grid::planar {

enum class direction {
    up,
    left,
    right,
    down,

    north = up,
    west = left,
    east = right,
    south = down,
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

struct extent {
    extent() = default;

    explicit extent(position p)
        : min_(p)
        , max_(p)
    {
    }

    explicit extent(position p, position q)
        : extent(p)
    {
        insert(q);
    }

    explicit extent(position p, offset u)
        : extent(p, p + u)
    {
    }

    template <typename mapped_type>
    explicit extent(const std::map<position, mapped_type>& grid)
    {
        for (const auto& [p, _] : grid) {
            insert(p);
        }
    }

    bool empty() const
    {
        return min_[0] > max_[0] || min_[1] > max_[1];
    }

    bool insert(position p);

    const position& min() const
    {
        return min_;
    }

    const position& max() const
    {
        return max_;
    }

private:
    position min_ { 0, 0 };
    position max_ { -1, -1 };
};

std::istream& operator>>(std::istream& in, direction& dir);

std::ostream& operator<<(std::ostream& out, offset u);
std::ostream& operator<<(std::ostream& out, position p);

// FIXME: read/write functions (with additional parameters) instead of these operators?
template <typename mapped_type>
std::ostream& operator<<(std::ostream& out, const std::tuple<const std::map<position, mapped_type>&, const extent&>& grid_extent)
{
    const auto [grid, extent] = grid_extent;
    const mapped_type empty {};
    for (position p = extent.min(); p[1] <= extent.max()[1]; ++p[1]) {
        for (p[0] = extent.min()[0]; p[0] <= extent.max()[0]; ++p[0]) {
            if (const auto it = grid.find(p); it != grid.end()) {
                out << it->second;
            } else {
                out << empty;
            }
        }
        out.put('\n');
    }
    return out;
}

template <typename mapped_type>
std::ostream& operator<<(std::ostream& out, const std::map<position, mapped_type>& grid)
{
    return out << std::tuple<const std::map<position, mapped_type>&, const extent&> { grid, extent(grid) };
}

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
