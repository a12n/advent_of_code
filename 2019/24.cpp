#include <cassert>
#include <set>

#include "grid.hpp"

namespace {

const size_t n = 5;

using bugs_grid = uint32_t;

constexpr bugs_grid bug(unsigned x, unsigned y)
{
    return 1 << (n * y + x);
}

constexpr bugs_grid at(bugs_grid bugs, unsigned x, unsigned y)
{
    return bugs & (1 << (n * y + x));
}

constexpr bugs_grid insert(bugs_grid bugs, unsigned x, unsigned y)
{
    return bugs | (1 << (n * y + x));
}

constexpr bugs_grid neighbors(bugs_grid bugs, unsigned x, unsigned y)
{
    return (y > 0 ? at(bugs, x, y - 1) : 0)
        | (x > 0 ? at(bugs, x - 1, y) : 0)
        | (x < n - 1 ? at(bugs, x + 1, y) : 0)
        | (y < n - 1 ? at(bugs, x, y + 1) : 0);
}

constexpr unsigned count(bugs_grid bugs)
{
    unsigned n = 0;
    while (bugs) {
        bugs &= (bugs - 1);
        ++n;
    }
    return n;
}

constexpr bugs_grid simulate(bugs_grid bugs)
{
    bugs_grid next = 0;
    for (unsigned y = 0; y < n; ++y) {
        for (unsigned x = 0; x < n; ++x) {
            const auto k = count(neighbors(bugs, x, y));
            if (at(bugs, x, y)) {
                if (k == 1) {
                    next = insert(next, x, y);
                }
            } else {
                if (k == 1 || k == 2) {
                    next = insert(next, x, y);
                }
            }
        }
    }
    return next;
}

constexpr size_t biodiversity(bugs_grid bugs)
{
    return bugs;
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
    bugs_grid bugs = 0;

    grid::planar::for_each(std::cin, [&bugs](auto p, auto c) {
        if (p[0] >= n || p[1] >= n || (c != '.' && c != '#')) {
            throw std::invalid_argument(__func__);
        }
        if (c == '#') {
            bugs = insert(bugs, p[0], p[1]);
        }
    });

    std::set<bugs_grid> seen;
    while (seen.find(bugs) == seen.end()) {
        seen.insert(bugs);
        bugs = simulate(bugs);
    }

    std::cout << biodiversity(bugs) << '\n';
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
#if PART == 1
    const bugs_grid bugs = bug(4, 0)
        | bug(0, 1) | bug(3, 1)
        | bug(0, 2) | bug(3, 2) | bug(4, 2)
        | bug(2, 3)
        | bug(0, 4);

    assert(neighbors(bugs, 0, 0) == bug(0, 1));
    assert(neighbors(bugs, 3, 2) == (bug(3, 1) | bug(4, 2)));

    assert(count(bugs) == 8);

    assert(biodiversity(bug(0, 3) | bug(1, 4)) == 2129920);
    assert(biodiversity(bugs) == 1205552);
#elif PART == 2
    const int16_t l = 0;
    {
        // Tile 19 has four adjacent tiles: 14, 18, 20, and 24.
        const bug_set adj = {
            { l, 3, 2 },
            { l, 2, 3 },
            { l, 4, 3 },
            { l, 3, 4 },
        };
        assert(adjacent({ l, 3, 3 }) == adj);
    }

    {
        // Tile G has four adjacent tiles: B, F, H, and L.
        const bug_set adj = {
            { l + 1, 1, 0 },
            { l + 1, 0, 1 },
            { l + 1, 2, 1 },
            { l + 1, 1, 2 },
        };
        assert(adjacent({ l + 1, 1, 1 }) == adj);
    }

    {
        // Tile D has four adjacent tiles: 8, C, E, and I.
        const bug_set adj = {
            { l, 2, 1 },
            { l + 1, 2, 0 },
            { l + 1, 4, 0 },
            { l + 1, 3, 1 },
        };
        assert(adjacent({ l + 1, 3, 0 }) == adj);
    }
#endif // PART
    return 0;
}
