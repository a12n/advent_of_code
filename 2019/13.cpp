#include <iostream>

#include <sysexits.h>

#include "ansi.hpp"
#include "intcode.hpp"

namespace {

enum class tile {
    empty = 0,
    wall = 1,
    block = 2,
    paddle = 3,
    ball = 4,
};

std::ostream& operator<<(std::ostream& out, tile t)
{
    switch (t) {
    case tile::empty:
        return out << ' ';
    case tile::wall:
        return out << '#';
    case tile::block:
        return out << 'H';
    case tile::paddle:
        return out << '-';
    case tile::ball:
        return out << '.';
    }
    return out << '?';
}

void arcade_cabinet(const intcode::memory& prog)
{
    enum {
        x_position,
        y_position,
        tile_id,
    } expect
        = x_position;

    ansi::cursor_position cursor_pos {};

    intcode::address ip = 0;
    intcode::memory img = prog;
    intcode::state st;

    while (true) {
        const auto [op, ip2, addr] = intcode::run_intrpt(st, img, ip);
        switch (op) {
        case intcode::opcode::output: {
            switch (expect) {
            case x_position:
                cursor_pos.col = img[addr];
                expect = y_position;
                break;
            case y_position:
                cursor_pos.row = img[addr];
                std::cout << cursor_pos;
                expect = tile_id;
                break;
            case tile_id:
                std::cout << tile(img[addr]);
                expect = x_position;
                break;
            }
        } break;
        case intcode::opcode::halt:
            return;
        default:
            throw std::runtime_error(__func__);
        }
        ip = ip2;
        std::cin.get();
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

    arcade_cabinet(img);

    return 0;
}
