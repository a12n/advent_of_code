#include <cassert>
#include <iostream>
#include <map>
#include <optional>

#include "grid.hpp"
#include "intcode.hpp"

namespace {

using namespace grid::planar;

enum class tile_type {
    wall = 0,
    empty = 1,
    oxygen = 2,
};

using tile_grid = std::map<position, tile_type>;

std::ostream& operator<<(std::ostream& out, tile_type tile)
{
    switch (tile) {
    case tile_type::wall:
        return out << "█";
    case tile_type::empty:
        return out << "░";
    case tile_type::oxygen:
        return out << "X";
    case tile_type(3):
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

tile_type move(repair_droid& droid, direction dir)
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
        return tile_type(droid.img[addr]);
    }
}

void explore(repair_droid& droid, tile_grid& tiles, position p, size_t steps)
{
    for (const direction dir : { direction::north, direction::west, direction::east, direction::south }) {
        const auto q = p + to_offset(dir);

        if (const auto it = tiles.find(q); it != tiles.end()) {
            continue;
        }

        const auto tile = move(droid, dir);
        tiles.insert({ q, tile });
        std::cerr << __func__ << " " << p << " -> " << q << " = " << tile << '\n';

        switch (tile) {
        case tile_type::wall:
            break;
        case tile_type::empty:
            explore(droid, tiles, q, steps + 1);
            move(droid, opposite(dir));
            break;
        case tile_type::oxygen:
            std::cerr << __func__ << " oxygen " << q << " " << steps + 1 << '\n';
            break;
        }
    };
}

tile_grid explore(const intcode::memory& prog)
{
    repair_droid droid = { prog };
    tile_grid tiles;

    tiles.insert({ { 0, 0 }, tile_type(3) });
    explore(droid, tiles, { 0, 0 }, 0);

    return grid;
}

} // namespace

int main(int argc, char* argv[])
{
    const auto img = intcode::load(argc, argv);
    const auto tiles = explore(img);

    std::cerr << output_grid<tile_type> { tiles, extent(tiles), tile_type(-1) };

    return 0;
}
