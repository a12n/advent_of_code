#include <array>
#include <cassert>
#include <cstdint>
#include <iostream>
#include <map>
#include <queue>
#include <stack>
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

size_t search(const vault_map& vault, position start, keys_set keys, keys_set target_keys)
{
    std::queue<std::tuple<size_t, position, keys_set>> states;
    std::set<std::tuple<position, keys_set>> seen;

    assert(at(vault, start, '#') != '#');
    states.push({ 0, start, keys });

    std::cerr << __func__ << ':'
              << " start " << start
              << " keys " << keys
              << " target_keys " << target_keys;
    while (!states.empty()) {
        auto [steps, p, keys] = states.front();
        states.pop();

        if (seen.find({ p, keys }) != seen.end()) {
            continue;
        }
        seen.insert({ p, keys });

        const auto c = at(vault, p, '#');

        if (door(c)) {
            if (!(door(c) & keys)) {
                continue;
            }
        } else if (key(c)) {
            keys |= key(c);
        }

        if ((keys & target_keys) == target_keys) {
            std::cerr << " steps " << steps << '\n';
            return steps;
        }

        for (const auto dir : { direction::up, direction::left, direction::right, direction::down }) {
            const auto q = p + to_offset(dir);

            if (at(vault, q, '#') == '#') {
                continue;
            }

            states.push({ steps + 1, q, keys });
        }
    }

    std::cerr << " infeasible\n";
    return -1;
}

std::tuple<keys_set, keys_set> reachable(const vault_map& vault, position p)
{
    keys_set doors = 0, keys = 0;
    sparse_set_grid seen;
    std::stack<position> states;

    states.push(p);

    while (!states.empty()) {
        const auto p = states.top();
        states.pop();

        if (seen.find(p) != seen.end()) {
            continue;
        }
        seen.insert(p);

        const auto c = at(vault, p, '#');

        doors |= door(c);
        keys |= key(c);

        for (const auto dir : { direction::up, direction::left, direction::right, direction::down }) {
            const auto q = p + to_offset(dir);

            if (at(vault, q, '#') == '#') {
                continue;
            }

            states.push(q);
        }
    }

    return { doors, keys };
}

keys_set reachable_keys(const vault_map& vault, position p)
{
    return std::get<1>(reachable(vault, p));
}

using position_array = std::array<position, 4>;

size_t search(const vault_map& vault, const position_array& start, keys_set all_keys)
{
    std::queue<std::tuple<size_t, size_t, position_array, keys_set, unsigned>> states;

    for (size_t j = 0; j < start.size(); ++j) {
        assert(at(vault, start[j], '#') != '#');
        states.push({ 0, j, start, all_keys, 0 });
    }

    size_t min_steps = -1;
    std::set<std::tuple<size_t, position, keys_set>> seen;

    while (!states.empty()) {
        auto [steps, i, p, no_keys, blocked] = states.front();
        states.pop();

        std::cerr << __func__ << ':'
                  << " steps " << steps
                  << " i " << i
                  << " pi " << p[i]
                  << " no_keys " << no_keys
                  << " blocked " << blocked
                  << " states " << states.size()
                  << " min_steps " << min_steps
                  << '\n';

        if (steps > min_steps) {
            continue;
        }

        if ((1 << i) & blocked) {
            continue;
        }

        // Had already seen this robot at this position with these
        // keys, drop the state.
        if (seen.find({ i, p[i], no_keys }) != seen.end()) {
            continue;
        }
        seen.insert({ i, p[i], no_keys });

        const auto c = at(vault, p[i], '#');

        if (door(c)) {
            if (!(door(c) & ~no_keys)) {
                blocked |= (1 << i);
            }
        } else if (no_keys & key(c)) {
            no_keys &= ~key(c);

            if (no_keys == 0) {
                if (steps < min_steps) {
                    min_steps = steps;
                }
                continue;
            }

            // If robot `j` was waiting at the door, allow it to try
            // to proceed with the new set of keys.
            for (size_t j = 0; j < start.size(); ++j) {
                if ((1 << j) & blocked) {
                    // XXX: Never happens?
                    states.push({ steps, j, p, no_keys, blocked & ~(1 << j) });
                }
            }
        }

        // Can switch from robot `i` to another robot `j` at door or
        // key position.
        if (key(c) || door(c)) {
            for (size_t j = 0; j < start.size(); ++j) {
                if (i != j && !((1 << j) & blocked)) {
                    states.push({ steps, j, p, no_keys, blocked });
                }
            }
        }

        // Continue with the robot `i` unless it's blocked at the locked door.
        if ((i << 1) & blocked) {
            continue;
        }

        for (const auto dir : { direction::up, direction::left, direction::right, direction::down }) {
            auto q = p;

            q[i] = p[i] + to_offset(dir);

            if (at(vault, q[i], '#') == '#') {
                continue;
            }

            states.push({ steps + 1, i, q, no_keys, blocked });
        }
    }

    return min_steps;
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
        all_keys |= key(c);
        return c;
    });

    const auto global_start = find<char>(vault, '@').value();

#if PART == 1
    std::cout << search(vault, global_start, 0, all_keys) << '\n';
#elif PART == 2
    for (int dy = -1; dy <= 1; ++dy) {
        for (int dx = -1; dx <= 1; ++dx) {
            set(vault, global_start + offset { dx, dy }, (dx == 0 || dy == 0) ? '#' : '@');
        }
    }

    const position start[4] = {
        global_start + offset { -1, -1 },
        global_start + offset { -1, +1 },
        global_start + offset { +1, -1 },
        global_start + offset { +1, +1 },
    };

    const keys_set keys[4] = {
        reachable_keys(vault, start[0]),
        reachable_keys(vault, start[1]),
        reachable_keys(vault, start[2]),
        reachable_keys(vault, start[3]),
    };

    assert((keys[0] | keys[1] | keys[2] | keys[3]) == all_keys);

    // Search independent parts of the vault separately. In each
    // sub-region pretend that keys from other parts are already
    // available.  XXX: Fails "sample-4" test.
    std::cout << search(vault, start[0], all_keys & ~keys[0], keys[0])
            + search(vault, start[1], all_keys & ~keys[1], keys[1])
            + search(vault, start[2], all_keys & ~keys[2], keys[2])
            + search(vault, start[3], all_keys & ~keys[3], keys[3])
              << '\n';
#endif // PART

    return 0;
}
