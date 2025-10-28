#include <iostream>
#include <map>
#include <optional>

#include <sysexits.h>

#include "grid.hpp"
#include "intcode.hpp"

namespace {

using namespace grid::planar;

enum color {
    black = 0,
    white = 1,
};

// Implicitly black.
using canvas = std::map<position, color>;

std::ostream& operator<<(std::ostream& out, color c)
{
    return out << (c == white ? "█" : c == black ? "░"
                                                 : "?");
}

canvas hull_painting_robot(const intcode::memory& prog, std::optional<color> start = std::nullopt)
{
    canvas canv;
    direction dir = direction::up;
    position p = { 0, 0 };
    enum {
        paint,
        turn,
    } expect
        = paint;

    intcode::memory img = prog;
    intcode::address ip = 0;
    intcode::value rel_base = 0;

    if (start) {
        canv[p] = *start;
    }

    while (true) {
        const auto [op, addr] = intcode::run_intrpt(img, ip, rel_base);
        switch (op) {
        case intcode::opcode::input:
            if (const auto it = canv.find(p); it != canv.end()) {
                img[addr] = it->second;
            } else {
                img[addr] = 0;
            }
            break;
        case intcode::opcode::output:
            if (expect == paint) {
                if (img[addr] == 0) {
                    canv[p] = black;
                } else if (img[addr] == 1) {
                    canv[p] = white;
                } else {
                    throw std::runtime_error(__func__);
                }
                expect = turn;
            } else if (expect == turn) {
                if (img[addr] == 0) {
                    dir = rotate(rotation::ccw, dir);
                } else if (img[addr] == 1) {
                    dir = rotate(rotation::cw, dir);
                } else {
                    throw std::runtime_error(__func__);
                }
                p += to_offset(dir);
                expect = paint;
            }
            break;
        case intcode::opcode::halt:
            return canv;
        default:
            throw std::runtime_error(__func__);
        }
    }
}

} // namespace

int main()
{
    using intcode::operator>>;

    intcode::memory img;

    if (!(std::cin >> img)) {
        return EX_DATAERR;
    }

#if PART == 1
    std::cout << hull_painting_robot(img).size() << '\n';
#elif PART == 2
    const auto canv = hull_painting_robot(img,white);
    std::cout << output_grid<color> { canv, extent(canv), black };
#endif // PART

    return 0;
}
