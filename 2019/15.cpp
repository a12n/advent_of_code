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
    }
    return out << "@";
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

size_t repair_droid(const intcode::memory& prog)
{
    intcode::memory img = prog;
    intcode::address ip = 0;
    intcode::value rel_base = 0;

    position p = { 0, 0 };
    std::map<position, std::optional<size_t>> steps;

    while (true) {
        const auto [op, addr] = intcode::run_intrpt(img, ip, rel_base);
        switch (op) {
        case intcode::opcode::input: {
            // TODO
        } break;
        case intcode::opcode::output: {
            // TODO
        } break;
        default:
            throw std::runtime_error(__func__);
        }
    }

    return 0;
}

} // namespace

int main(int argc, char* argv[])
{
    const auto img = intcode::load(argc, argv);

    std::cout << repair_droid(img) << '\n';

    return 0;
}
