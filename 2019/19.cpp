#include <cassert>
#include <deque>
#include <iostream>

#include "grid.hpp"
#include "intcode.hpp"

using namespace grid::planar;

int main(int argc, char* argv[])
{
    const intcode::memory prog = intcode::load(argc, argv);

#if PART == 1
    std::deque<position> queue;
    sparse_set_grid beam;

    // Primer positions.
    for (int64_t y = 0; y < 10; ++y) {
        for (int64_t x = 0; x < 10; ++x) {
            queue.push_back({ x, y });
        }
    }

    while (!queue.empty()) {
        const auto p = queue.front();
        queue.pop_front();

        if (p[0] >= 50 || p[1] >= 50 || beam.find(p) != beam.end()) {
            continue;
        }

        intcode::memory img = prog;
        intcode::address ip = 0;
        intcode::value rel_base = 0;

        {
            const auto [op, addr] = intcode::run_intrpt(img, ip, rel_base);
            assert(op == intcode::opcode::input);
            img[addr] = p[0];
        }

        {
            const auto [op, addr] = intcode::run_intrpt(img, ip, rel_base);
            assert(op == intcode::opcode::input);
            img[addr] = p[1];
        }

        {
            const auto [op, addr] = intcode::run_intrpt(img, ip, rel_base);
            assert(op == intcode::opcode::output);
            assert(img[addr] == 0 || img[addr] == 1);
            if (img[addr]) {
                queue.push_back(p + offset { 0, 1 });
                queue.push_back(p + offset { 1, 0 });
                queue.push_back(p + offset { 1, 1 });
                beam.insert(p);
            }
        }

        {
            const auto [op, addr] = intcode::run_intrpt(img, ip, rel_base);
            assert(op == intcode::opcode::halt);
        }
    }

    output(std::cerr, beam, extent(beam.begin(), beam.end()), ' ', '#');
    std::cout << beam.size() << '\n';
#endif // PART

    return 0;
}
