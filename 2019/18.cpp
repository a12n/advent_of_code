#include <cstdint>
#include <iostream>
#include <map>
#include <stdexcept>
#include <unordered_map>

#include "grid.hpp"

namespace {

using namespace grid::planar;

// 1 << (c - 'a') for keys [a-z]
using keys_set = uint32_t;

constexpr keys_set key(char c)
{
    if (c < 'a' || c > 'z') {
        return 0;
    }
    return 1 << (c - 'a');
}

constexpr keys_set door(char c)
{
    if (c < 'A' || c > 'Z') {
        return 0;
    }
    return 1 << (c - 'A');
}

using vault_map = dense_grid<char>;

size_t search(const vault_map& vault, position start, keys_set all_keys)
{
    std::set<std::tuple<position, keys_set>> seen;
    std::set<std::tuple<size_t, position, keys_set>> states;

    states.insert({ 0, start, all_keys });

    while (!states.empty()) {
        auto [steps, p, no_keys] = *states.begin();
        states.erase(states.begin());

        if (seen.find({ p, no_keys }) != seen.end()) {
            continue;
        }
        seen.insert({ p, no_keys });

        if (const auto c = at(vault, p, '#'); c == '#') {
            continue;
        } else if (door(c)) {
            if (!(door(c) & ~no_keys)) {
                continue;
            }
        } else if (key(c)) {
            no_keys &= ~key(c);
            if (no_keys == 0) {
                return steps;
            }
        }

        for (const auto dir : { direction::up, direction::left, direction::right, direction::down }) {
            states.insert({ steps + 1, p + to_offset(dir), no_keys });
        }
    }

    return -1;
}

} // namespace

int main()
{
    vault_map vault;
    keys_set all_keys = 0;

    input(std::cin, vault, [&all_keys](position, char c) {
        if (c != '@' && c != '.' && c != '#' && (c < 'a' || c > 'z') && (c < 'A' && c > 'Z')) {
            throw std::invalid_argument(__func__);
        }
        if (key(c)) {
            all_keys |= key(c);
        }
        return c;
    });

    const auto start = find<char>(vault, '@').value();

#if PART == 1
    std::cout << search(vault, start, all_keys) << '\n';
#endif // PART == 1

    return 0;
}
