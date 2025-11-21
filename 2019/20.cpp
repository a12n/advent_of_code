#include <array>
#include <iostream>

#include "grid.hpp"

using namespace grid::planar;

using label_type = std::array<char, 2>;

int main(void)
{
    dense_grid<char> grid;

    input<char>(std::cin, grid, [](position, char c) {
        if (c != ' ' && c != '.' && c != '#' && !(c >= 'A' && c <= 'Z')) {
            throw std::invalid_argument(__func__);
        }
        return c;
    });
    output(std::cerr, grid);

    for_each(grid, [&grid](position p, char label1) {
        if (label1 >= 'A' && label1 <= 'Z') {
            if (const auto label2 = at(grid, p + offset { 1, 0 }, ' '); label2 >= 'A' && label2 <= 'Z') {
                const auto label = label_type { label1, label2 };
                if (const auto q = p - offset { 1, 0 }; at(grid, q, ' ') == '.') {
                    // label += q
                    std::cerr << label1 << label2 << '=' << q << '\n';
                } else if (const auto q = p + offset { 2, 0 }; at(grid, q, ' ') == '.') {
                    // label += q
                    std::cerr << label1 << label2 << '=' << q << '\n';
                }
            } else if (const auto label2 = at(grid, p + offset { 0, 1 }, ' '); label2 >= 'A' && label2 <= 'Z') {
                const auto label = label_type { label1, label2 };
                if (const auto q = p - offset { 0, 1 }; at(grid, q, ' ') == '.') {
                    // label += q
                    std::cerr << label1 << label2 << '=' << q << '\n';
                } else if (const auto q = p + offset { 0, 2 }; at(grid, q, ' ') == '.') {
                    // label += q
                    std::cerr << label1 << label2 << '=' << q << '\n';
                }
            }
        }
    });

    return 0;
}
