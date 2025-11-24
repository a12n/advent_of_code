#include <array>
#include <cassert>
#include <deque>
#include <iostream>
#include <tuple>

#include "intcode.hpp"

namespace {

const size_t n = 50;

using packet = std::array<intcode::value, 2>;

intcode::value network(const intcode::memory& nic)
{
    std::array<intcode::memory, n> img;
    std::array<intcode::address, n> ip {};
    std::array<intcode::value, n> rel_base {};
    std::array<std::deque<packet>, n> queue;

    // Load program.
    for (size_t i = 0; i < n; ++i) {
        img[i] = nic;
    }

    intcode::opcode op;
    intcode::address addr;

    // Assign addresses.
    for (size_t i = 0; i < n; ++i) {
        std::tie(op, addr) = intcode::run_intrpt(img[i], ip[i], rel_base[i]);
        assert(op == intcode::opcode::input);
        img[i][addr] = i;
    }

    // Run computers.
    while (true) {
        for (size_t i = 0; i < n; ++i) {
            std::tie(op, addr) = intcode::run_intrpt(img[i], ip[i], rel_base[i]);
            assert(op == intcode::opcode::input || op == intcode::opcode::output);
            if (op == intcode::opcode::output) {
                // Send packet.
                const auto dest = img[i][addr];
                std::tie(op, addr) = intcode::run_intrpt(img[i], ip[i], rel_base[i]);
                assert(op == intcode::opcode::output);
                const auto x = img[i][addr];
                std::tie(op, addr) = intcode::run_intrpt(img[i], ip[i], rel_base[i]);
                assert(op == intcode::opcode::output);
                const auto y = img[i][addr];
                if (dest == 255) {
                    return y;
                } else {
                    queue[dest].push_back({ x, y });
                }
            } else if (op == intcode::opcode::input) {
                // Receive packet.
                if (queue[i].empty()) {
                    img[i][addr] = -1;
                } else {
                    const auto [x, y] = queue[i].front();
                    img[i][addr] = x;
                    std::tie(op, addr) = intcode::run_intrpt(img[i], ip[i], rel_base[i]);
                    assert(op == intcode::opcode::input);
                    img[i][addr] = y;
                    queue[i].pop_front();
                }
            }
        }
    }
}

} // namespace

int main(int argc, char* argv[])
{
    auto img = intcode::load(argc, argv);
    std::cout << network(img) << '\n';
    return 0;
}
