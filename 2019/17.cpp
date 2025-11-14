#include <cassert>
#include <iostream>
#include <vector>

#include "grid.hpp"
#include "intcode.hpp"

namespace {

using namespace grid::planar;

using scaffold_view = std::vector<std::vector<char>>;

struct ascii_prog {
    intcode::memory img;
    intcode::address ip = 0;
    intcode::value rel_base = 0;
};

void render(ascii_prog& ascii, scaffold_view& view, position p)
{
    view.push_back({});
    while (true) {
        const auto [op, addr] = intcode::run_intrpt(ascii.img, ascii.ip, ascii.rel_base);
        switch (op) {
        case intcode::opcode::output:
            if (ascii.img[addr] == '\n') {
                view.push_back({});
                p = { 0, p[1] + 1 };
            } else {
                view.back().push_back(ascii.img[addr]);
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

size_t calibrate_cameras(const scaffold_view& view)
{
    size_t sum = 0;
    for (size_t i = 0; i < view.size(); ++i) {
        if (i == 0 || i == (view.size() - 1)) {
            continue;
        }
        for (size_t j = 0; j < view[i].size(); ++j) {
            if (j == 0 || j == (view[i].size() - 1)) {
                continue;
            }
            if (view[i][j] != '.' && (view[i - 1][j] != '.' && view[i][j - 1] != '.' && view[i][j + 1] != '.' && view[i + 1][j] != '.')) {
                sum += i * j;
            }
        }
    }
    return sum;
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

int test()
{
#if PART == 1
    assert(
        calibrate_cameras({
            { '.', '.', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.' },
            { '.', '.', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.' },
            { '#', '#', '#', '#', '#', '#', '#', '.', '.', '.', '#', '#', '#' },
            { '#', '.', '#', '.', '.', '.', '#', '.', '.', '.', '#', '.', '#' },
            { '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#' },
            { '.', '.', '#', '.', '.', '.', '#', '.', '.', '.', '#', '.', '.' },
            { '.', '.', '#', '#', '#', '#', '#', '.', '.', '.', '^', '.', '.' },
        })
        == 76);
#endif // PART
    return 0;
}
