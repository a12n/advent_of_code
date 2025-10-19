#ifndef ADVENT_HPP
#define ADVENT_HPP

#include <algorithm>
#include <array>
#include <cassert>
#include <charconv>
#include <cstdint>
#include <cstdlib>
#include <forward_list>
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

#include <sysexits.h>

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

namespace grid::planar {

enum class direction {
    up,
    left,
    right,
    down,
};

struct offset : public std::array<int64_t, 2> { };
struct position : public std::array<int64_t, 2> { };

direction opposite(direction dir);
int64_t taxicab_norm(offset u);
direction to_direction(char c);
position to_position(offset u);
offset to_offset(direction dir);
offset to_offset(position p);

offset& operator*=(offset& u, int64_t n);
offset operator*(int64_t n, offset u);
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
// IntCode Computer

namespace intcode {

using address = size_t;
using value = int64_t;

enum class opcode {
    add = 1,
    mul = 2,
    input = 3,
    output = 4,
    jump_if_true = 5,
    jump_if_false = 6,
    less_than = 7,
    equals = 8,
    halt = 99,
};

enum class mode {
    position = 0,
    immediate = 1,
};

using memory = std::vector<value>;

struct environ {
    virtual value input();
    virtual void output(value v);
};

struct test_environ : environ {
    value input() override;
    void output(value v) override;

    std::forward_list<value> fake_in;
    std::forward_list<value> expected_out;
};

address run(memory& img , address ip);
address run(memory& img, address ip, environ& env);

std::istream& operator>>(std::istream& in, memory& img);

void test(
    const memory& prog,
    address start, address stop,
    const std::forward_list<value>& in, const std::forward_list<value>& out);

} // namespace intcode

#endif // ADVENT_HPP
