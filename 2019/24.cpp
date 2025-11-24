#include <set>

#include "grid.hpp"

namespace {

using namespace grid::planar;

using bugs_grid = fixed_grid<bool, 5, 5>;

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

} // namespace

int main()
{
    bugs_grid grid;

    input(std::cin, grid, [](position, char c) { return c == '#'; });

    std::set<bugs_grid> seen;
    while (seen.find(grid) == seen.end()) {
        seen.insert(grid);
        grid = simulate(grid);
    }

    std::cout << biodiversity(grid) << '\n';

    return 0;
}
