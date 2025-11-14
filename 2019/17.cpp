#include <iostream>
#include <map>

#include "grid.hpp"
#include "intcode.hpp"

namespace {

using namespace grid::planar;

using scaffold_view = std::map<position, char>;

struct ascii_prog {
    intcode::memory img;
    intcode::address ip = 0;
    intcode::value rel_base = 0;
};

void render(ascii_prog& ascii, scaffold_view& view, position p)
{
    while (true) {
        const auto [op, addr] = intcode::run_intrpt(ascii.img, ascii.ip, ascii.rel_base);
        switch (op) {
        case intcode::opcode::output:
            if (ascii.img[addr] == '\n') {
                p = { 0, p[1] + 1 };
            } else {
                view.insert({ p, ascii.img[addr] });
                ++p[0];
            }
            break;
        case intcode::opcode::halt:
            return;
        default:
            throw std::invalid_argument(__func__);
        }
    }
}

} // namespace

int main(int argc, char* argv[])
{
    scaffold_view view;

    {
        ascii_prog ascii = { intcode::load(argc, argv) };
        render(ascii, view, { 0, 0 });
    }

    std::cerr << output_grid<char> { view, extent(view), ' ' };

    return 0;
}
