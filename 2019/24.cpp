#include <cassert>
#include <set>

#include "grid.hpp"

namespace {

const size_t n = 5;
using namespace grid::planar;

#if PART == 1
using bugs_grid = fixed_grid<bool, n, n>;

bugs_grid simulate(const bugs_grid& grid)
{
    bugs_grid next;
    for (position p = { 0, 0 }; p[1] < 5; ++p[1]) {
        for (p[0] = 0; p[0] < 5; ++p[0]) {
            const size_t k = at(grid, p + to_offset(direction::up)) + at(grid, p + to_offset(direction::left)) + at(grid, p + to_offset(direction::right)) + at(grid, p + to_offset(direction::down));
            if (at(grid, p)) {
                next[p[1]][p[0]] = (k == 1);
            } else {
                next[p[1]][p[0]] = (k == 1 || k == 2);
            }
        }
    }
    return next;
}

size_t biodiversity(const bugs_grid& grid)
{
    size_t mult = 1;
    size_t sum = 0;
    for (const auto& row : grid) {
        for (const auto bug : row) {
            if (bug) {
                sum += mult;
            }
            mult *= 2;
        }
    }
    return sum;
}

#elif PART == 2
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
            func(bug_position ( level, x, y - 1 ));
        }
    } else if (dir == direction::left) {
        if (x == 0) {
            func(bug_position(level-1,(n/2)-1,n/2));
        } else if (x == (n / 2) + 1 && y == (n / 2)) {
            for_each_adjacent_edge(dir, level + 1, func);
        } else {
            // func(bug_position(p_level, p + to_offset(dir)));
            func(bug_position ( level,x - 1, y ));
        }
    } else if (dir == direction::right) {
        if (x == n - 1) {
            func(bug_position(level - 1, (n / 2) + 1, (n / 2)));
        } else if (x == (n / 2) - 1 && y == (n / 2)) {
            for_each_adjacent_edge(dir, level + 1, func);
        } else {
            // func(bug_position(p_level, p + to_offset(dir)));
            func(bug_position ( level, x + 1, y ));
        }
    } else if (dir == direction::down) {
        if (y == n - 1) {
            func(bug_position(level - 1, (n / 2), (n / 2) + 1));
        } else if (x == (n / 2) && y == (n / 2) - 1) {
            for_each_adjacent_edge(dir, level + 1, func);
        } else {
            // func(bug_position(p_level, p + to_offset(dir)));
            func(bug_position ( level, x, y + 1 ));
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
    bugs_grid grid;

    input(std::cin, grid, [](position, char c) { return c == '#'; });

    std::set<bugs_grid> seen;
    while (seen.find(grid) == seen.end()) {
        seen.insert(grid);
        grid = simulate(grid);
    }

    std::cout << biodiversity(grid) << '\n';
#elif PART == 2
    // int test(void);
    // return test();
    bug_set grid;

    for_each(std::cin, [&grid](position p, char c) {
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

    // for (const auto& [p_level, p] : grid) {
    //     std::cerr << p_level << ' ' << p << '\n';
    // }

    std::cout << grid.size() << '\n';
#endif // PART

    return 0;
}

int test()
{
    const int16_t l=0;
#if PART == 2
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
