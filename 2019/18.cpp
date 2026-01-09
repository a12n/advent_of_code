#include <array>
#include <cassert>
#include <cstdint>
#include <iostream>
#include <map>
#include <stdexcept>
#include <unordered_map>

#include "grid.hpp"

namespace {

using namespace grid::planar;

namespace keys {

// 1 << (c - 'a') for keys [a-z]
using set = uint32_t;

constexpr set key(char c)
{
    if (c < 'a' || c > 'z') {
        return 0;
    }
    return 1 << (c - 'a');
}

constexpr set door(char c)
{
    if (c < 'A' || c > 'Z') {
        return 0;
    }
    return 1 << (c - 'A');
}

} // namespace keys

// Move in DFS order from @ in all directions
// If X encountered, (if stack not empty, add rule "x -> lowercase top of the stack"), put X into the stack
// If x encountered, (if stack not empty, add rule "x -> lowercase top of the stack")

const size_t n = 1 + ('z' - 'a') + ('Z' - 'A');

using graph = std::unordered_map<char, std::unordered_map<char, size_t>>;

void to_graph(graph& g, const dense_grid<char>& grid, position p, sparse_set_grid& visited, size_t n, std::string path)
{
    visited.insert(p);

    for (const auto dir : { direction::up, direction::left, direction::right, direction::down }) {
        const auto q = p + to_offset(dir);

        if (contains(visited, q)) {
            continue;
        }

        const auto c = at(grid, q, '#');

        if (c == '#') {
            continue;
        }

        if (c == '.') {
            to_graph(g, grid, q, visited, n + 1, path);
        } else {
            std::cerr << "	\"" << path.back() << "\" -- \"" << c << "\" [label=\"" << n + 1 << "\"];\n";
            g[c][path.back()] = n + 1;
            g[path.back()][c] = n + 1;
            to_graph(g, grid, q, visited, 0, path + c);
        }
    }

    visited.erase(p);
}

void to_graph(graph& g, const dense_grid<char>& grid, position start)
{
    assert(at(grid, start) == '@');
    g.clear();
    sparse_set_grid visited;
    std::cerr << "strict graph {\n";
    to_graph(g, grid, start, visited, 0, "@");
    std::cerr << "}\n";
}

void normalize_graph(graph& g)
{
    for (auto& [u, adj] : g) {
        for (auto& [v, _] : adj) {
            if (keys::door(v) && g.at(v).size() == 1) {
                std::cout << "Remove " << v << '\n';
            }
        }
    }
}

using distance_map = std::map<char, std::map<char, size_t>>;

using vault_map = dense_grid<char>;

distance_map distances(const vault_map& vault)
{
    // TODO
    return {};
}

void dfs(const vault_map& vault, sparse_set_grid& visited, size_t& min_steps, size_t steps, position p, keys::set keys)
{
    if (steps >= min_steps) {
        return;
    }

    visited.insert(p);

    if (const auto c = at(vault, p, '#'); keys::key(c)) {
        if (const auto k = keys::key(c); (keys & k)) {
            keys &= ~k;

            if (keys == 0) {
                // Found all keys.
                visited.erase(p);
                std::cerr << __func__ << ':' << __LINE__ << " found all keys, steps " << steps << '\n';
                min_steps = steps;
                return;
            } else {
                sparse_set_grid visited_from_p;
                dfs(vault, visited_from_p, min_steps, steps, p, keys);
            }
        }
    }

    for (const auto dir : { direction::up, direction::left, direction::right, direction::down }) {
        const auto q = p + to_offset(dir);

        // Already visited.
        if (visited.find(q) != visited.end()) {
            continue;
        }

        const auto c = at(vault, q, '#');

        if (c == '#') {
            continue;
        }

        if (keys::door(c) && (keys & keys::door(c))) {
            continue;
        }

        dfs(vault, visited, min_steps, steps + 1, q, keys);
    }
    visited.erase(p);
}

size_t dfs(const vault_map& vault, position p, keys::set keys)
{
    size_t min_steps = -1;
    sparse_set_grid visited;
    dfs(vault, visited, min_steps, 0, p, keys);
    return min_steps;
}

size_t search(const graph& graph, const keys::set all_keys, char u = '@')
{
    std::set<std::tuple<size_t, char, keys::set>> states;
    size_t min_steps = -1;

    states.insert({ 0, u, 0 });

    while (!states.empty()) {
        const auto [total_steps, u, keys] = *states.begin();
        states.erase(states.begin());

        std::cerr << __func__ << ": total_steps " << total_steps << " u " << u << " keys " << keys << " states " << states.size() << '\n';

        if (total_steps > min_steps) {
            continue;
        }

        if ((keys & all_keys) == all_keys && total_steps < min_steps) {
            min_steps = total_steps;
        }

        for (const auto& [v, steps] : graph.at(u)) {
            if (v == '@' || (keys::door(v) && (keys & keys::door(v)))) {
                states.insert({ total_steps + steps, v, keys });
            } else if (keys::key(v)) {
                states.insert({ total_steps + steps, v, keys | keys::key(v) });
            }
        }
    }

    return min_steps;
}

} // namespace

int main()
{
    graph graph;
    keys::set all_keys = 0;

    {
        dense_grid<char> grid;

        input(std::cin, grid, [&all_keys](position, char c) {
            if (c != '@' && c != '.' && c != '#' && (c < 'a' || c > 'z') && (c < 'A' && c > 'Z')) {
                throw std::invalid_argument(__func__);
            }
            if (keys::key(c)) {
                all_keys |= keys::key(c);
            }
            return c;
        });

        std::cerr << __func__ << ": all_keys " << all_keys << '\n';

        to_graph(graph, grid, find<char>(grid, '@').value());
    }

    std::cout << search(graph, all_keys, '@') << '\n';

    return 0;
}
