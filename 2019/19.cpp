#include <cassert>
#include <iostream>

#include "intcode.hpp"

int main(int argc, char* argv[])
{
    intcode::memory img = intcode::load(argc, argv);
    intcode::address ip = 0;
    intcode::value rel_base = 0;

#if PART == 1
    size_t pulled = 0;

    for (size_t y = 0; y < 50; ++y) {
        for (size_t x = 0; x < 50; ++x) {
            {
                const auto [op, addr] = intcode::run_intrpt(img, ip, rel_base);
                assert(op == intcode::opcode::input);
                img[addr] = x;
            }

            {
                const auto [op, addr] = intcode::run_intrpt(img, ip, rel_base);
                assert(op == intcode::opcode::input);
                img[addr] = y;
            }

            {
                const auto [op, addr] = intcode::run_intrpt(img, ip, rel_base);
                assert(op == intcode::opcode::output);
                assert(img[addr] == 0 || img[addr] == 1);
                if (img[addr]) {
                    ++pulled;
                    std::cerr.put('#');
                } else {
                    std::cerr.put('.');
                }
            }
        }
        std::cerr.put('\n');
    }

    std::cout << pulled << '\n';
#endif // PART

    return 0;
}
