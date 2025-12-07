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

// 1 << (c - 'a') for keys [a-z]
using key_set = uint32_t;

constexpr bool is_key(char k)
{
    return k >= 'a' && k <= 'z';
}

constexpr bool is_door(char d)
{
    return d >= 'A' && d <= 'Z';
}

constexpr size_t to_index(char c)
{
    if (c >= '@' && c <= 'Z') {
        return (c - '@');
    } else if (c >= 'a' && c <= 'z') {
        return ('Z' - '@') + 1 + (c - 'a');
    } else {
        throw std::invalid_argument(__func__);
    }
}

constexpr key_set from_key(char k)
{
    return 1 << (k - 'a');
}

constexpr key_set from_door(char d)
{
    return 1 << (d - 'A');
}

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

using distance_map = std::map<char, std::map<char, size_t>>;

using vault_map = dense_grid<char>;

distance_map distances(const vault_map& vault)
{
    // TODO
    return {};
}

void dfs(const vault_map& vault, sparse_set_grid& visited, size_t& min_steps, size_t steps, position p, key_set keys)
{
    if (steps >= min_steps) {
        return;
    }

    visited.insert(p);

    if (const auto c = at(vault, p, '#'); is_key(c)) {
        if (const auto k = from_key(c); (keys & k)) {
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

        if (is_door(c) && (keys & from_door(c))) {
            continue;
        }

        dfs(vault, visited, min_steps, steps + 1, q, keys);
    }
    visited.erase(p);
}

size_t dfs(const vault_map& vault, position p, key_set keys)
{
    size_t min_steps = -1;
    sparse_set_grid visited;
    dfs(vault, visited, min_steps, 0, p, keys);
    return min_steps;
}

} // namespace

int main()
{
    graph graph;
    position start;

    {
        dense_grid<char> grid;

        input(std::cin, grid, [](position, char c) {
            if (c == '@' || c == '.' || c == '#' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
                return c;
            }
            throw std::invalid_argument(__func__);
        });

        start = find<char>(grid, '@').value();
        to_graph(graph, grid, start);
    }

    return 0;
}
