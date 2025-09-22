#ifndef ADVENT_HPP
#define ADVENT_HPP

#include <algorithm>
#include <array>
#include <charconv>
#include <cstdint>
#include <cstdlib>
#include <functional>
#include <iomanip>
#include <iostream>
#include <istream>
#include <map>
#include <optional>
#include <ostream>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

//----------------------------------------------------------------------------
// I/O Utilities

template <typename item_type, char sep = ','>
std::istream& operator>>(std::istream& in, std::vector<item_type>& items)
{
    items.clear();

    item_type v;

    if (!(in >> v)) {
        return in;
    }
    items.push_back(std::move(v));

    while (true) {
        char c;

        if (!(in >> c)) {
            if (in.eof()) {
                in.clear(in.rdstate() & ~std::ios::failbit);
            }
            break;
        }

        if (c != sep) {
            in.unget();
            break;
        }

        if (!(in >> v)) {
            in.setstate(std::ios::failbit);
            break;
        }
        items.push_back(std::move(v));
    }

    return in;
}

//----------------------------------------------------------------------------
// 2D Grids

namespace grid {

enum class direction {
    up,
    left,
    right,
    down,
};

struct point : public std::array<int64_t, 2> { };
struct vector : public std::array<int64_t, 2> { };

int64_t taxicab_norm(vector u);
direction to_direction(char c);
point to_point(vector u);
vector to_vector(direction dir);
vector to_vector(point p);

vector operator*(int64_t n, vector u);
point operator+(point p, vector u);
vector operator+(vector u, vector v);
vector operator-(point p, point q);
point operator-(point p, vector u);
vector operator-(vector u);

std::istream& operator>>(std::istream& in, direction& dir);

} // namespace grid

template <>
struct std::hash<grid::point> {
    size_t operator()(grid::point p) const noexcept
    {
        return std::hash<int64_t> {}(p[0]) ^ (std::hash<int64_t> {}(p[1]) << 1);
    }
};

template <>
struct std::hash<grid::vector> {
    size_t operator()(grid::vector u) const noexcept
    {
        return std::hash<grid::point> {}(to_point(u));
    }
};

//----------------------------------------------------------------------------
// IntCode Computer

namespace intcode {

using address = size_t;

enum class opcode {
    add = 1,
    mul = 2,
    halt = 99,
};

using memory = std::vector<int64_t>;

address run(memory& m, address ip);

std::istream& operator>>(std::istream& in, memory& m);

} // namespace intcode

#endif // ADVENT_HPP
