#include <iostream>
#include <map>
#include <optional>

#include "grid.hpp"
#include "intcode.hpp"

namespace {

using namespace grid::planar;

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
