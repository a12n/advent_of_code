#include <cassert>
#include <set>

#include "grid.hpp"

namespace bugs {

// The size of the Eris bugs grid is n×n.
const size_t n = 5;

//----------------------------------------------------------------------------
// Single-level bugs grid

// 25 bits to represent presence of bugs on the 5×5 grid.
using set = uint32_t;

// Single bit for a bug at the specified position.
constexpr set bug(unsigned x, unsigned y)
{
    return 1 << (n * y + x);
}

// Bugs at all the positions of the speicified row number.
constexpr set row(unsigned y)
{
    return bug(0, y) | bug(1, y) | bug(2, y) | bug(3, y) | bug(4, y);
}

// Bugs at all the positions of the speicified column number.
constexpr set column(unsigned x)
{
    return bug(x, 0) | bug(x, 1) | bug(x, 2) | bug(x, 3) | bug(x, 4);
}

// Get bug at the specified position on the grid.
constexpr set at(set b, unsigned x, unsigned y)
{
    return b & bug(x, y);
}

// Get bugs of the speicified row in the grid.
constexpr set row_at(set b, unsigned y)
{
    return at(b, 0, y) | at(b, 1, y) | at(b, 2, y) | at(b, 3, y) | at(b, 4, y);
}

// Get bugs of the speicified column.
constexpr set column_at(set b, unsigned x)
{
    return at(b, x, 0) | at(b, x, 1) | at(b, x, 2) | at(b, x, 3) | at(b, x, 4);
}

// Set bug at the specified position.
constexpr set insert(set b, unsigned x, unsigned y)
{
    return b | bug(x, y);
}

// Usual 4-neighborhood of a bug at specified position.
constexpr set neighbors(set b, unsigned x, unsigned y)
{
    return (y > 0 ? at(b, x, y - 1) : 0)
        | (x > 0 ? at(b, x - 1, y) : 0)
        | (x < n - 1 ? at(b, x + 1, y) : 0)
        | (y < n - 1 ? at(b, x, y + 1) : 0);
}

// For each present bug in the set, call the provided function.
template <typename func_type>
constexpr void for_each_bug(set b, func_type f)
{
    for (unsigned y = 0; y < n; ++y) {
        for (unsigned x = 0; x < n; ++x) {
            if (b & 1) {
                f(x, y);
            }
            if (!(b >>= 1)) {
                return;
            }
        }
    }
}

//----------------------------------------------------------------------------
// Grid of bugs in the recursively-folded space

// Bugs grid on multiple levels.
using multiset = std::vector<set>;

// Mapping of levels (negative and non-negative) to indices into the
// `multiset` array of separate grids.
constexpr unsigned index(int level)
{
    //  0 -> 0
    //  1 -> 1
    // -1 -> 2
    //  2 -> 3
    // -2 -> 4
    //  3 -> 5
    // -3 -> 6
    // …
    if (level < 0) {
        return 2 * -level;
    } else if (level > 0) {
        return 2 * level - 1;
    } else {
        return 0;
    }
}

// Inverse of index.
constexpr int unindex(unsigned i)
{
    if (i & 1) {
        return (i + 1) / 2;
    } else if (i) {
        return -(i / 2);
    } else {
        return 0;
    }
}

set at(const multiset& b, int level, unsigned x, unsigned y)
{
    const auto i = index(level);
    if (i < b.size()) {
        return b[i];
    }
    return 0;
}

void insert(multiset& b, int level, unsigned x, unsigned y)
{
    const auto i = index(level);
    if (i >= b.size()) {
        b.resize(i + 1, 0);
    }
    b[i] = insert(b[i], x, y);
}


// Neighbors at the same level will be stored in the `immed` set (at
// index 0), neighbors from the middle tile will be in the `inner` set
// (at index 1), and neighbors from the outer grid will be in `outer`
// set (index 2).
constexpr std::array<set, 3> neighbors(std::array<set, 3> b, unsigned x, unsigned y)
{
    const unsigned mid = n / 2;

    std::array<set, 3> ans {};
    enum {
        immed,
        inner,
        outer,
    };

    // Up
    if (y == 0) {
        ans[outer] |= at(b[outer], mid, mid - 1);
    } else if (x == mid && y == (mid + 1)) {
        ans[inner] |= row_at(b[inner], n - 1);
    } else {
        ans[immed] |= at(b[immed], x, y - 1);
    }

    // Left
    if (x == 0) {
        ans[outer] |= at(b[outer], mid - 1, mid);
    } else if (x == (mid + 1) && y == mid) {
        ans[inner] |= column_at(b[inner], n - 1);
    } else {
        ans[immed] |= at(b[immed], x - 1, y);
    }

    // Right
    if (x == n - 1) {
        ans[outer] |= at(b[outer], mid + 1, mid);
    } else if (x == (mid - 1) && y == mid) {
        ans[inner] |= column_at(b[inner], 0);
    } else {
        ans[immed] |= at(b[immed], x + 1, y);
    }

    // Down
    if (y == n - 1) {
        ans[outer] |= at(b[outer], mid, mid + 1);
    } else if (x == mid && y == (mid - 1)) {
        ans[inner] |= row_at(b[inner], 0);
    } else {
        ans[immed] |= at(b[immed], x, y + 1);
    }

    return ans;
}

// Count bugs on the grid.
constexpr unsigned count(set b)
{
    unsigned n = 0;
    while (b) {
        b &= (b - 1);
        ++n;
    }
    return n;
}

// Count bugs on all non-empty levels of the multi-level grid.
unsigned count(const multiset& b)
{
    unsigned n = 0;
    for (const auto bi : b) {
        n += count(bi);
    }
    return n;
}

} // namespace bugs

namespace {

constexpr bugs::set simulate(bugs::set b)
{
    bugs::set next = 0;
    for (unsigned y = 0; y < bugs::n; ++y) {
        for (unsigned x = 0; x < bugs::n; ++x) {
            const auto k = bugs::count(bugs::neighbors(b, x, y));
            if (bugs::at(b, x, y)) {
                if (k == 1) {
                    next = bugs::insert(next, x, y);
                }
            } else {
                if (k == 1 || k == 2) {
                    next = bugs::insert(next, x, y);
                }
            }
        }
    }
    return next;
}

constexpr size_t biodiversity(bugs::set b)
{
    return b;
}

#if PART == 2
using grid::planar::direction;

// Level and grid position.
using bug_position = std::tuple<int16_t, int8_t, int8_t>;
using bug_set = std::set<bug_position>;

// Next positions on the grid of adjacent level, from the specified
// direction.
template <typename func_type>
void for_each_adjacent_edge(direction dir, int64_t level, func_type func)
{
    for (int64_t i = 0; i < n; ++i) {
        func(dir == direction::up         ? bug_position(level, i, n - 1)
                : dir == direction::left  ? bug_position(level, n - 1, i)
                : dir == direction::right ? bug_position(level, 0, i)
                : dir == direction::down  ? bug_position(level, i, 0)
                                          : throw std::invalid_argument(__func__));
    }
}

// Next positions for the position p at the specified level to the
// particular direction.
template <typename func_type>
void for_each_adjacent_dir(const bug_position& bug, direction dir, func_type func)
{
    const auto [level, x, y] = bug;
    if (dir == direction::up) {
        if (y == 0) {
            func(bug_position(level - 1, n / 2, (n / 2) - 1));
        } else if (x == (n / 2) && y == (n / 2) + 1) {
            for_each_adjacent_edge(dir, level + 1, func);
        } else {
            // func(bug_position(p_level, p + to_offset(dir)));
            func(bug_position(level, x, y - 1));
        }
    } else if (dir == direction::left) {
        if (x == 0) {
            func(bug_position(level - 1, (n / 2) - 1, n / 2));
        } else if (x == (n / 2) + 1 && y == (n / 2)) {
            for_each_adjacent_edge(dir, level + 1, func);
        } else {
            // func(bug_position(p_level, p + to_offset(dir)));
            func(bug_position(level, x - 1, y));
        }
    } else if (dir == direction::right) {
        if (x == n - 1) {
            func(bug_position(level - 1, (n / 2) + 1, (n / 2)));
        } else if (x == (n / 2) - 1 && y == (n / 2)) {
            for_each_adjacent_edge(dir, level + 1, func);
        } else {
            // func(bug_position(p_level, p + to_offset(dir)));
            func(bug_position(level, x + 1, y));
        }
    } else if (dir == direction::down) {
        if (y == n - 1) {
            func(bug_position(level - 1, (n / 2), (n / 2) + 1));
        } else if (x == (n / 2) && y == (n / 2) - 1) {
            for_each_adjacent_edge(dir, level + 1, func);
        } else {
            // func(bug_position(p_level, p + to_offset(dir)));
            func(bug_position(level, x, y + 1));
        }
    }
}

// Next positions for the specified position on this and adjacent
// levels.
template <typename func_type>
void for_each_adjacent(const bug_position& bug, func_type func)
{
    for (const auto dir : { direction::up, direction::left, direction::right, direction::down }) {
        for_each_adjacent_dir(bug, dir, func);
    }
}

bug_set adjacent(const bug_position& bug)
{
    bug_set adj;
    std::cerr << __func__ << ": {" << std::get<0>(bug) << ' ' << int(std::get<1>(bug)) << ' ' << int(std::get<2>(bug)) << "} = [";
    for_each_adjacent(bug, [&adj](const bug_position& p) {
        std::cerr << " {" << std::get<0>(p) << ' ' << int(std::get<1>(p)) << ' ' << int(std::get<2>(p)) << "}";
        adj.insert(p);
    });
    std::cerr << " ]\n";
    return adj;
}

bug_set simulate(const bug_set& grid)
{
    bug_set next;
    bug_set maybe_infested;

    for (const auto& bug : grid) {
        size_t n_neighbors = 0;

        for_each_adjacent(bug, [&grid, &maybe_infested, &n_neighbors](const bug_position& neighbor) {
            if (grid.find(neighbor) != grid.end()) {
                ++n_neighbors;
            } else {
                // TODO: Insert the next loop right here.
                maybe_infested.emplace(neighbor);
            }
        });

        if (n_neighbors == 1) {
            next.emplace(bug);
        }
    }

    for (const auto& bug : maybe_infested) {
        size_t n_neighbors = 0;

        for_each_adjacent(bug, [&grid, &n_neighbors](const bug_position& neighbor) {
            if (grid.find(neighbor) != grid.end()) {
                ++n_neighbors;
            }
        });

        if (n_neighbors == 1 || n_neighbors == 2) {
            next.emplace(bug);
        }
    }

    return next;
}

#endif // PART

} // namespace

int main()
{
#if PART == 1
    bugs::set b = 0;

    grid::planar::for_each(std::cin, [&b](auto p, auto c) {
        if (p[0] >= bugs::n || p[1] >= bugs::n || (c != '.' && c != '#')) {
            throw std::invalid_argument(__func__);
        }
        if (c == '#') {
            b = bugs::insert(b, p[0], p[1]);
        }
    });

    std::set<bugs::set> seen;
    while (seen.find(b) == seen.end()) {
        seen.insert(b);
        b = simulate(b);
    }

    std::cout << biodiversity(b) << '\n';
#elif PART == 2
    // int test(void);
    // return test();
    bug_set grid;

    grid::planar::for_each(std::cin, [&grid](auto p, auto c) {
        if (p[0] >= n || p[1] >= n || (c != '.' && c != '#')) {
            throw std::invalid_argument(__func__);
        }
        if (c == '#') {
            // grid.insert(bug_position(0,p));
            grid.insert(bug_position(0, p[0], p[1]));
        }
    });

    size_t minutes = 200;

    if (const auto minutes_env = getenv("MINUTES"); minutes_env) {
        minutes = std::stol(minutes_env);
    }

    while (minutes-- > 0) {
        grid = simulate(grid);
    }

    std::cout << grid.size() << '\n';
#endif // PART

    return 0;
}

int test()
{
    using namespace bugs;

    const set sample = bug(4, 0)
        | bug(0, 1) | bug(3, 1)
        | bug(0, 2) | bug(3, 2) | bug(4, 2)
        | bug(2, 3)
        | bug(0, 4);

    assert(neighbors(sample, 0, 0) == bug(0, 1));
    assert(neighbors(sample, 3, 2) == (bug(3, 1) | bug(4, 2)));

    assert(count(sample) == 8);

    assert(biodiversity(bug(0, 3) | bug(1, 4)) == 2129920);
    assert(biodiversity(sample) == 1205552);

    {
        set check = 0;
        for_each_bug(sample, [&check](unsigned x, unsigned y) {
            check = insert(check, x, y);
        });
        assert(check == sample);
    }

    {
        assert(index(0) == 0);
        assert(index(1) == 1);
        assert(index(-1) == 2);
        assert(index(2) == 3);
        assert(index(-2) == 4);
        for (int lvl = -100; lvl <= 100; ++lvl) {
            assert(unindex(index(lvl)) == lvl);
        }
    }


    return 0;
}
