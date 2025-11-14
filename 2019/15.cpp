#include <algorithm>
#include <cassert>
#include <deque>
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
using distance_grid = std::map<position, size_t>;

std::ostream& operator<<(std::ostream& out, tile_type tile)
{
    switch (tile) {
    case tile_type::wall:
        return out << "██";
    case tile_type::empty:
        return out << "░░";
    case tile_type::oxygen:
        return out << "OO";
    case tile_type(3):
        return out << "@@";
    }
    return out << "▓▓";
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

void explore(repair_droid& droid, tile_grid& tiles, std::optional<position>& oxygen, position p)
{
    for (const direction dir : { direction::north, direction::west, direction::east, direction::south }) {
        const auto q = p + to_offset(dir);

        if (const auto it = tiles.find(q); it != tiles.end()) {
            continue;
        }

        const auto tile = move(droid, dir);
        tiles.insert({ q, tile });

        switch (tile) {
        case tile_type::wall:
            break;
        case tile_type::empty:
            explore(droid, tiles, oxygen, q);
            move(droid, opposite(dir));
            break;
        case tile_type::oxygen:
            if (!oxygen) {
                oxygen = q;
            }
            explore(droid, tiles, oxygen, q);
            move(droid, opposite(dir));
            break;
        }
    };
}

void explore(repair_droid& droid, tile_grid& tiles, std::optional<position>& oxygen)
{
    tiles.clear();
    oxygen.reset();
    tiles.insert({ { 0, 0 }, tile_type(3) });
    explore(droid, tiles, oxygen, { 0, 0 });
}

distance_grid distances(const tile_grid& tiles, position src)
{
    distance_grid dists;

    dists.insert({ src, 0 });

    std::deque<position> states;

    states.push_back(src);
    while (!states.empty()) {
        const auto p = states.front();
        states.pop_front();

        const auto dist = dists.find(p)->second;

        for (const direction dir : { direction::north, direction::west, direction::east, direction::south }) {
            const auto q = p + to_offset(dir);

            if (const auto it = tiles.find(q); it == tiles.end() || it->second == tile_type::wall) {
                continue;
            }

            if (const auto it = dists.find(q); it != dists.end()) {
                continue;
            }

            dists.insert({ q, dist + 1 });
            states.push_back(q);
        }
    }

    return dists;
}

} // namespace

int main(int argc, char* argv[])
{
    tile_grid tiles;
    std::optional<position> oxygen;

    {
        repair_droid droid = { intcode::load(argc, argv) };
        explore(droid, tiles, oxygen);
    }

    const auto dists = distances(tiles, *oxygen);

#if PART == 1
    std::cout << dists.at({ 0, 0 }) << '\n';
#elif PART == 2
    std::cout << std::max_element(dists.begin(), dists.end(), [](const auto& a, const auto& b) {
        return a.second < b.second;
    })->second << '\n';
#endif // PART

    return 0;
}
