#include <array>
#include <cassert>
#include <deque>
#include <iostream>
#include <optional>
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

    // NAT.
    std::optional<packet> nat, nat_sent;

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
        bool idle = true;

        for (size_t i = 0; i < n; ++i) {
            std::tie(op, addr) = intcode::run_intrpt(img[i], ip[i], rel_base[i]);
            assert(op == intcode::opcode::input || op == intcode::opcode::output);
            if (op == intcode::opcode::output) {
                idle = false;
                // Send packet.
                const auto dest = img[i][addr];
                std::tie(op, addr) = intcode::run_intrpt(img[i], ip[i], rel_base[i]);
                assert(op == intcode::opcode::output);
                const auto x = img[i][addr];
                std::tie(op, addr) = intcode::run_intrpt(img[i], ip[i], rel_base[i]);
                assert(op == intcode::opcode::output);
                const auto y = img[i][addr];
                if (dest == 255) {
#if PART == 1
                    return y;
#elif PART == 2
                    nat = { x, y };
#endif // PART
                } else {
                    queue[dest].push_back({ x, y });
                }
            } else if (op == intcode::opcode::input) {
                // Receive packet.
                if (queue[i].empty()) {
                    img[i][addr] = -1;
                } else {
                    idle = false;
                    const auto [x, y] = queue[i].front();
                    img[i][addr] = x;
                    std::tie(op, addr) = intcode::run_intrpt(img[i], ip[i], rel_base[i]);
                    assert(op == intcode::opcode::input);
                    img[i][addr] = y;
                    queue[i].pop_front();
                }
            }
        }

        if (idle && nat.has_value()) {
            const auto [_, y0] = *nat;
            if (nat_sent) {
                const auto [_, y1] = *nat_sent;
                if (y0 == y1) {
                    return y0;
                }
            }
            queue[0].push_back(*nat);
            nat_sent = *nat;
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
