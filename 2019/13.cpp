#include <iostream>
#include <optional>

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
        return out << " ";
    case tile::wall:
        return out << "█";
    case tile::block:
        return out << "░";
    case tile::paddle:
        return out << "▔";
    case tile::ball:
        return out << "o";
    };
    return out << "?";
}

std::pair<size_t, size_t> arcade_cabinet(
    const intcode::memory& prog,
    std::optional<size_t> coins = {},
    std::ostream& screen = std::cerr)
{
    enum class expect {
        x,
        y,
        tile,
    } expect
        = expect::x;
    intcode::value x = 0, y = 0;
    intcode::value x_ball = 0, x_paddle = 0;

    intcode::address ip = 0;
    intcode::memory img = prog;
    intcode::state st;

    size_t n_block = 0, score = 0;

    if (coins) {
        img[0] = *coins;
    }

    screen << ansi::erase {} << ansi::hide_cursor {};
    while (true) {
        const auto [op, ip2, addr] = intcode::run_intrpt(st, img, ip);
        switch (op) {
        case intcode::opcode::input: {
            const auto x_diff = x_ball - x_paddle;
            img[addr] = (x_diff < 0) ? -1 : (x_diff > 0) ? 1
                                                         : 0;
        } break;
        case intcode::opcode::output: {
            switch (expect) {
            case expect::x:
                x = img[addr];
                expect = expect::y;
                break;
            case expect::y:
                y = img[addr];
                expect = expect::tile;
                break;
            case expect::tile:
                if (x == -1 && y == 0) {
                    score = img[addr];
                    screen << ansi::cursor_position { 0, 41 } << score;
                } else if (x >= 0 && y >= 0) {
                    const auto til = tile(img[addr]);
                    if (til == tile::block) {
                        ++n_block;
                    } else if (til == tile::paddle) {
                        x_paddle = x;
                    } else if (til == tile::ball) {
                        x_ball = x;
                    }
                    screen << ansi::cursor_position {
                        static_cast<size_t>(y),
                        static_cast<size_t>(x)
                    } << til;
                } else {
                    throw std::invalid_argument(__func__);
                }
                expect = expect::x;
                break;
            }
        } break;
        case intcode::opcode::halt:
            screen << ansi::hide_cursor { false };
            return { n_block, score };
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

#if PART == 1
    std::cout << arcade_cabinet(img).first << '\n';
#elif PART == 2
    std::cout << arcade_cabinet(img, 2).second << '\n';
#endif // PART

    return 0;
}
