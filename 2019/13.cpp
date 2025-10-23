#include <iostream>

#include <sysexits.h>

#include "intcode.hpp"

namespace {

enum class tile {
    empty = 0,
    wall = 1,
    block = 2,
    paddle = 3,
    ball = 4,
};

enum class expect {
    x,
    y,
    tile,
};

#if PART == 1
size_t arcade_cabinet(const intcode::memory& prog)
{
    expect expect = expect::x;
    size_t n_block = 0;

    intcode::address ip = 0;
    intcode::memory img = prog;
    intcode::state st;

    while (true) {
        const auto [op, ip2, addr] = intcode::run_intrpt(st, img, ip);
        switch (op) {
        case intcode::opcode::output: {
            switch (expect) {
            case expect::x:
                expect = expect::y;
                break;
            case expect::y:
                expect = expect::tile;
                break;
            case expect::tile:
                n_block += (tile(img[addr]) == tile::block);
                expect = expect::x;
                break;
            }
        } break;
        case intcode::opcode::halt:
            return n;
        default:
            throw std::runtime_error(__func__);
        }
        ip = ip2;
    }
}
#elif PART == 2
#endif // PART

} // namespace

int main(int argc, char* argv[])
{
    const auto img = intcode::load(argc, argv);

#if PART == 1
    std::cout << arcade_cabinet(img) << '\n';
#endif // PART

    return 0;
}
