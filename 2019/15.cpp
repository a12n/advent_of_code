#include <iostream>

#include "intcode.hpp"

namespace {

size_t repair_droid(const intcode::memory& prog)
{
    intcode::address ip = 0;
    intcode::memory img = prog;
    intcode::state st;

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
}

} // namespace

int main(int argc, char* argv[])
{
    const auto img = intcode::load(argc, argv);

    std::cout << repair_droid(img) << '\n';

    return 0;
}
