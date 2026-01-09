#ifndef GRID_HPP
#define GRID_HPP

#include <algorithm>
#include <array>
#include <cstdint>
#include <istream>
#include <map>
#include <optional>
#include <ostream>
#include <set>
#include <stdexcept>
#include <string_view>
#include <vector>

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

    right = cw,
    left = ccw,
};

struct offset : public std::array<int64_t, 2> { };
struct position : public std::array<int64_t, 2> { };

std::string_view direction_symbol(direction dir, bool unicode = true);
std::string_view rotation_symbol(rotation rdir, bool unicode = true);

direction opposite(direction dir);
int64_t taxicab_norm(offset u);
direction to_direction(char c);
position to_position(offset u);
offset to_offset(direction dir);
offset to_offset(position p);
offset normalize(offset u);
direction rotate(rotation rdir, direction dir);
offset rotate(rotation rdir, offset u);
double angle(offset u);
position midpoint(position p, position q);

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

template <typename mapped_type, size_t n, size_t m>
using fixed_grid = std::array<std::array<mapped_type, m>, n>;

template <typename mapped_type>
using dense_grid = std::vector<std::vector<mapped_type>>;

template <typename mapped_type>
using sparse_grid = std::map<position, mapped_type>;
using position_set = std::set<position>;
using sparse_set_grid = position_set;

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
    explicit extent(const sparse_grid<mapped_type>& grid)
    {
        for (const auto& [p, _] : grid) {
            insert(p);
        }
    }

    template <typename mapped_type>
    explicit extent(const dense_grid<mapped_type>& grid)
    {
        for (const auto& row : grid) {
            if (!row.empty()) {
                max_[0] = std::max(max_[0], static_cast<int64_t>(row.size() - 1));
            };
        }
        if (max_[0] != -1) {
            max_[1] = grid.size() - 1;
        }
    }

    template <typename iterator>
    explicit extent(iterator begin, iterator end)
    {
        for (; begin != end; ++begin) {
            insert(*begin);
        }
    }

    bool empty() const
    {
        return min_[0] > max_[0] || min_[1] > max_[1];
    }

    bool insert(position p);

    position min() const
    {
        return min_;
    }

    position max() const
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

template <typename mapped_type>
const mapped_type& at(const sparse_grid<mapped_type>& grid, position p, const mapped_type& empty = mapped_type())
{
    if (const auto it = grid.find(p); it != grid.end()) {
        return it->second;
    }
    return empty;
}

inline bool contains(const sparse_set_grid& grid, position p)
{
    return grid.find(p) != grid.end();
}

template <typename mapped_type>
const mapped_type& at(const dense_grid<mapped_type>& grid, position p, const mapped_type& empty = mapped_type())
{
    if (p[1] >= 0 && p[1] < grid.size()) {
        if (const auto& row = grid[p[1]]; p[0] >= 0 && p[0] < row.size()) {
            return row[p[0]];
        }
    }
    return empty;
}

template <typename mapped_type>
void set(dense_grid<mapped_type>& grid, position p, const mapped_type& v)
{
    grid[p[1]][p[0]] = v;
}

template <typename mapped_type, size_t n, size_t m>
const mapped_type& at(const fixed_grid<mapped_type, n, m>& grid, position p, const mapped_type& empty = mapped_type())
{
    if (p[1] >= 0 && p[1] < n && p[0] >= 0 && p[0] < m) {
        return grid[p[1]][p[0]];
    }
    return empty;
}

// Call func for each character in a grid read from the input stream.
template <typename func_type>
void for_each(std::istream& s, func_type func)
{
    for (position p = { 0, 0 }; s.good(); ++p[1]) {
        for (p[0] = 0;; ++p[0]) {
            if (const auto c = s.get(); c != '\n' && c != -1) {
                func(p, c);
            } else {
                break;
            }
        }
    }
    if (!s.eof()) {
        throw std::invalid_argument(__func__);
    }
}

// Input sparse grid from istream, mapping characters to the
// mapped_type values with a func.
template <typename mapped_type, typename func_type>
void input(std::istream& s, sparse_grid<mapped_type>& grid, func_type func)
{
    grid.clear();
    for_each(s, [&grid, func](position p, char c) {
        grid.insert({ p, func(p, c) });
    });
}

// Same as input for sparse_grid above, but for dense_grid.
template <typename mapped_type, typename func_type>
void input(std::istream& s, dense_grid<mapped_type>& grid, func_type func)
{
    grid.clear();
    for_each(s, [&grid, func](position p, char c) {
        if (p[1] == grid.size()) {
            grid.emplace_back();
        }
        grid.back().push_back(func(p, c));
    });
}

// Same as input for dense_grid, but for fixed_grid.
template <typename mapped_type, size_t n, size_t m, typename func_type>
void input(std::istream& s, fixed_grid<mapped_type, n, m>& grid, func_type func)
{
    for_each(s, [&grid, func](position p, char c) {
        if (p[1] >= n || p[0] >= m) {
            throw std::invalid_argument(__func__);
        }
        grid[p[1]][p[0]] = func(p, c);
    });
}

template <typename mapped_type>
void output(std::ostream& s, const sparse_grid<mapped_type>& grid, const extent& ext, const mapped_type& empty)
{
    for (position p = ext.min(); p[1] <= ext.max()[1]; ++p[1]) {
        for (p[0] = ext.min()[0]; p[0] <= ext.max()[0]; ++p[0]) {
            if (const auto it = grid.find(p); it != grid.end()) {
                s << it->second;
            } else {
                s << empty;
            }
        }
        s.put('\n');
    }
}

template <typename mapped_type>
void output(std::ostream& s, const sparse_set_grid& grid, const extent& ext, const mapped_type& empty, const mapped_type& non_empty)
{
    for (position p = ext.min(); p[1] <= ext.max()[1]; ++p[1]) {
        for (p[0] = ext.min()[0]; p[0] <= ext.max()[0]; ++p[0]) {
            if (const auto it = grid.find(p); it != grid.end()) {
                s << non_empty;
            } else {
                s << empty;
            }
        }
        s.put('\n');
    }
}

template <typename mapped_type, typename pred_type>
std::optional<position> find_if(const dense_grid<mapped_type>& grid, pred_type pred)
{
    for (position p = { 0, 0 }; static_cast<size_t>(p[1]) < grid.size(); ++p[1]) {
        for (p[0] = 0; static_cast<size_t>(p[0]) < grid[p[1]].size(); ++p[0]) {
            if (pred(p, grid[p[1]][p[0]])) {
                return p;
            }
        }
    }
    return {};
}

template <typename mapped_type>
std::optional<position> find(const dense_grid<mapped_type>& grid, const mapped_type& v)
{
    return find_if<mapped_type>(grid, [&v](position p, const mapped_type& u) { return u == v; });
}

template <typename mapped_type, typename func_type>
void for_each(const dense_grid<mapped_type>& grid, func_type func)
{
    find_if<mapped_type>(grid, [func](position p, const auto& v) {
        func(p, v);
        return false;
    });
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
