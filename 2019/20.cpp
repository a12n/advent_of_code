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

    std::map<label_type, std::vector<position>> portal_labels;
    for_each(grid, [&grid, &portal_labels](position p, char label1) {
        if (label1 >= 'A' && label1 <= 'Z') {
            if (const auto label2 = at(grid, p + offset { 1, 0 }, ' '); label2 >= 'A' && label2 <= 'Z') {
                const auto label = label_type { label1, label2 };
                if (const auto q = p - offset { 1, 0 }; at(grid, q, ' ') == '.') {
                    portal_labels[label].push_back(q);
                } else if (const auto q = p + offset { 2, 0 }; at(grid, q, ' ') == '.') {
                    portal_labels[label].push_back(q);
                }
            } else if (const auto label2 = at(grid, p + offset { 0, 1 }, ' '); label2 >= 'A' && label2 <= 'Z') {
                const auto label = label_type { label1, label2 };
                if (const auto q = p - offset { 0, 1 }; at(grid, q, ' ') == '.') {
                    portal_labels[label].push_back(q);
                } else if (const auto q = p + offset { 0, 2 }; at(grid, q, ' ') == '.') {
                    portal_labels[label].push_back(q);
                }
            }
        }
    });

    position start, finish;
    std::map<position, position> portals;
    for (const auto& [label, positions] : portal_labels) {
        if (positions.size() == 2) {
            portals.insert({ positions[0], positions[1] });
            portals.insert({ positions[1], positions[0] });
        } else if (positions.size() == 1) {
            if (label == label_type { 'A', 'A' }) {
                start = positions[0];
            } else if (label == label_type { 'Z', 'Z' }) {
                finish = positions[0];
            } else {
                std::invalid_argument(__func__);
            }
        } else {
            throw std::invalid_argument(__func__);
        }
    }

    std::cerr << "start " << start << '\n'
              << "finish " << finish << '\n';
    for (const auto& [from, to] : portals) {
        std::cerr << from << " -> " << to << '\n';
    }

    return 0;
}
