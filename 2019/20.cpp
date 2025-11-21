#include <array>
#include <deque>
#include <iostream>

#include "grid.hpp"

namespace {

using namespace grid::planar;

using label_type = std::array<char, 2>;

sparse_grid<size_t> distances(const dense_grid<char>& grid, const sparse_grid<position>& portals, position start)
{
    const size_t invalid_dist = -1;

    sparse_grid<size_t> dists;
    std::deque<position> queue;

    dists.insert({ start, 0 });
    queue.push_back(start);

    while (!queue.empty()) {
        const auto p = queue.front();
        queue.pop_front();

        for (const auto dir : { direction::up, direction::left, direction::right, direction::down }) {
            auto q = p + to_offset(dir);

            // Teleport if it's a portal.
            q = at(portals, q, q);

            // Don't step into walls.
            if (at(grid, q) != '.') {
                continue;
            }

            // Ignore positions for which distance is already known.
            if (at(dists, q, invalid_dist) != invalid_dist) {
                continue;
            }

            dists.emplace(q, dists.at(p) + 1);
            queue.push_back(q);
        }
    }

    return dists;
}

} // namespace

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

    using portal_exit = std::tuple<position, direction>;

    std::map<label_type, std::vector<portal_exit>> portal_exits;
    for_each(grid, [&grid, &portal_exits](position lp1, char l1) {
        if (l1 < 'A' || l1> 'Z') {
            return;
        }

        for (const auto dir : {direction::right,direction::down}){
            const auto lp2 = lp1 + to_offset(dir);
            const auto l2 = at(grid,lp2,' ');

            if (l2 < 'A' || l2 >'Z'){
                continue;
            }

            for (const auto& [p, dir2] : { portal_exit { lp1, opposite(dir) }, portal_exit { lp2, dir } }) {
                if (at(grid, p + to_offset(dir2), ' ') != '.') {
                    continue;
                }

                portal_exits[{ l1, l2 }].emplace_back(p, dir2);
            }
        }
    });

    position start, finish;
    std::map<position, position> portals;
    for (const auto& [label, exits] : portal_exits) {
        if (exits.size() == 2) {
            const auto& [p1, dir1] = exits[0];
            const auto& [p2, dir2] = exits[1];

            portals.insert({ p1, p2 + to_offset(dir2) });
            portals.insert({ p2, p1 + to_offset(dir1) });
        } else if (exits.size() == 1) {
            const auto& [p, dir] = exits[0];

            if (label == label_type { 'A', 'A' }) {
                start = p + to_offset(dir);
            } else if (label == label_type { 'Z', 'Z' }) {
                finish = p + to_offset(dir);
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

    std::cout << distances(grid, portals, start).at(finish) << '\n';

    return 0;
}
