#include <cassert>
#include <deque>
#include <iostream>

#include "grid.hpp"
#include "intcode.hpp"

namespace {

using namespace grid::planar;

std::optional<position> drone_system(const intcode::memory&prog, sparse_set_grid& beam, size_t limit = static_cast<size_t>(-1))
{
    beam.clear();

    std::deque<position> queue;

    // Primer positions.
    for (int64_t y = 0; y < 10; ++y) {
        for (int64_t x = 0; x < 10; ++x) {
            queue.push_back({ x, y });
        }
    }

    while (!queue.empty()) {
        const auto p = queue.front();
        queue.pop_front();

        if (p[0] >= limit || p[1] >= limit || beam.find(p) != beam.end()) {
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
                queue.push_back(p + offset { 1, 0 });
                queue.push_back(p + offset { 0, 1 });
                queue.push_back(p + offset { 1, 1 });
                beam.insert(p);

                const auto top_right = p - offset { 0, 100 - 1 };
                const auto bottom_left = p - offset { 100 - 1, 0 };
                const auto top_left = p - offset { 100 - 1, 100 - 1 };
                if (beam.find(top_right) != beam.end() && beam.find(bottom_left) != beam.end() && beam.find(top_left) != beam.end()) {
                    return top_left;
                }
            }
        }

        {
            const auto [op, addr] = intcode::run_intrpt(img, ip, rel_base);
            assert(op == intcode::opcode::halt);
        }
    }

    return {};
}

} // namespace

int main(int argc, char* argv[])
{
    sparse_set_grid beam;

#if PART == 1
    drone_system(intcode::load(argc, argv), beam, 50);
    std::cout << beam.size() << '\n';
#elif PART == 2
    const auto top_left = drone_system(intcode::load(argc, argv), beam).value();
    std::cout << 10000 * top_left[0] + top_left[1] << '\n';
#endif // PART

    return 0;
}
