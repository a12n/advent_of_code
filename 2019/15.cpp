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
    intcode::address ip = 0;
    intcode::memory img = prog;
    intcode::state st;

    position p = { 0, 0 };
    std::map<position, std::optional<size_t>> steps;

    while (true) {
        const auto [op, ip2, addr] = intcode::run_intrpt(st, img, ip);
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
        ip = ip2;
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
