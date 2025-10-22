#include <iostream>
#include <map>

#include <sysexits.h>

#include "advent.hpp"

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
    return out << (c == white ? '#' : c == black ? '.'
                                                 : '_');
}

std::ostream& operator<<(std::ostream& out, const canvas& canv)
{
    if (canv.empty()) {
        return out;
    }

    const auto p_min = canv.begin()->first;
    const auto p_max = canv.rbegin()->first;

    std::cerr << __func__ << " min " << p_min << " max " << p_max << '\n';

    // TODO: Iterate over canvas points.
    for (position p = p_min; p[1] <= p_max[1]; ++p[1]) {
        for (p[0] = p_min[0]; p[0] <= p_max[0]; ++p[0]) {
            color c = black;
            if (const auto it = canv.find(p); it != canv.end()) {
                c = it->second;
            }
            out << c;
        }
        out.put('\n');
    }

    return out;
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

    intcode::address ip = 0;
    intcode::memory img = prog;
    intcode::state st;

    if (start) {
        canv[{ 0, 0 }] = *start;
    }

    while (true) {
        const auto [op, ip2, addr] = intcode::run_intrpt(st, img, ip);
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
        ip = ip2;
    }
}

} // namespace

int main()
{
    intcode::memory img;

    if (!(std::cin >> img)) {
        return EX_DATAERR;
    }

    const auto canv =
#if PART == 1
        hull_painting_robot(img)
#elif PART == 2
        hull_painting_robot(img, white)
#endif // PART
        ;
    std::cerr << canv;
    std::cout << canv.size() << '\n';

    return 0;
}
