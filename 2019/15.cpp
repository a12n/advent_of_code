#include <cassert>
#include <iostream>
#include <map>
#include <optional>

#include "grid.hpp"
#include "intcode.hpp"

namespace {

using namespace grid::planar;

enum class tile {
    wall = 0,
    empty = 1,
    oxygen = 2,
};

std::ostream& operator<<(std::ostream& out, tile t)
{
    switch (t) {
    case tile::wall:
        return out << "█";
    case tile::empty:
        return out << "░";
    case tile::oxygen:
        return out << "X";
    case tile(3):
        return out << "@";
    }
    return out << " ";
}

intcode::value movement_command(direction dir)
{
    switch (dir) {
    case direction::north:
        return 1;
    case direction::west:
        return 3;
    case direction::east:
        return 4;
    case direction::south:
        return 2;
    }
    throw std::invalid_argument(__func__);
}

tile move(intcode::memory& img, intcode::address& ip, intcode::value& rel_base, direction dir)
{
    // Input movement command
    {
        const auto [op, addr] = intcode::run_intrpt(img, ip, rel_base);
        assert(op == intcode::opcode::input);
        img[addr] = movement_command(dir);
    }

    // Output status
    {
        const auto [op, addr] = intcode::run_intrpt(img, ip, rel_base);
        assert(op == intcode::opcode::output);
        assert(img[addr] >= 0 && img[addr] <= 2);
        return tile(img[addr]);
    }
}

void explore(intcode::memory& img, intcode::address& ip, intcode::value& rel_base, std::map<position, tile>& grid, position p, size_t steps)
{
    for (const direction dir : { direction::north, direction::west, direction::east, direction::south }) {
        const auto q = p + to_offset(dir);

        if (const auto it = grid.find(q); it != grid.end()) {
            continue;
        }

        const auto til = move(img, ip, rel_base, dir);
        grid.insert({ q, til });
        std::cerr << __func__ << " " << p << " -> " << q << " = " << til << '\n';

        switch (til) {
        case tile::wall:
            break;
        case tile::empty:
            explore(img, ip, rel_base, grid, q, steps + 1);
            move(img, ip, rel_base, opposite(dir));
            break;
        case tile::oxygen:
            std::cerr << __func__ << " oxygen " << q << " " << steps + 1 << '\n';
            break;
        }
    };
}

std::map<position, tile> explore(const intcode::memory& prog)
{
    std::map<position, tile> grid;

    intcode::memory img = prog;
    intcode::address ip = 0;
    intcode::value rel_base = 0;

    grid[{ 0, 0 }] = tile(3);
    explore(img, ip, rel_base, grid, { 0, 0 }, 0);

    return grid;
}

} // namespace

int main(int argc, char* argv[])
{
    const auto img = intcode::load(argc, argv);

    const auto grid = explore(img);

    std::cerr << output_grid<tile> { grid, extent(grid), tile(-1) };

    return 0;
}
