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

struct repair_droid {
    intcode::memory img;
    intcode::address ip = 0;
    intcode::value rel_base = 0;
};

tile move(repair_droid& droid, direction dir)
{
    // Input movement command
    {
        const auto [op, addr] = intcode::run_intrpt(droid.img, droid.ip, droid.rel_base);
        assert(op == intcode::opcode::input);
        droid.img[addr] = movement_command(dir);
    }

    // Output status
    {
        const auto [op, addr] = intcode::run_intrpt(droid.img, droid.ip, droid.rel_base);
        assert(op == intcode::opcode::output);
        assert(droid.img[addr] >= 0 && droid.img[addr] <= 2);
        return tile(droid.img[addr]);
    }
}

void explore(repair_droid& droid, std::map<position, tile>& grid, position p, size_t steps)
{
    for (const direction dir : { direction::north, direction::west, direction::east, direction::south }) {
        const auto q = p + to_offset(dir);

        if (const auto it = grid.find(q); it != grid.end()) {
            continue;
        }

        const auto til = move(droid, dir);
        grid.insert({ q, til });
        std::cerr << __func__ << " " << p << " -> " << q << " = " << til << '\n';

        switch (til) {
        case tile::wall:
            break;
        case tile::empty:
            explore(droid, grid, q, steps + 1);
            move(droid, opposite(dir));
            break;
        case tile::oxygen:
            std::cerr << __func__ << " oxygen " << q << " " << steps + 1 << '\n';
            break;
        }
    };
}

std::map<position, tile> explore(const intcode::memory& prog)
{
    repair_droid droid = { prog };
    std::map<position, tile> grid;

    grid[{ 0, 0 }] = tile(3);
    explore(droid, grid, { 0, 0 }, 0);

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
