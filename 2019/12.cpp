#include <algorithm>
#include <array>
#include <iostream>
#include <numeric>
#include <optional>
#include <sstream>

#include <sysexits.h>

#include "grid.hpp"

namespace {

using namespace grid::spatial;

template <size_t n>
using position_array = std::array<position, n>;

template <size_t n>
using velocity_array = std::array<offset, n>;

int64_t potential_energy(const position& p)
{
    return taxicab_norm(to_offset(p));
}

int64_t kinetic_energy(const offset& v)
{
    return taxicab_norm(v);
}

int64_t total_energy(const position& p, const offset& v)
{
    return potential_energy(p) * kinetic_energy(v);
}

template <size_t n>
int64_t total_energy(const position_array<n>& p, const velocity_array<n>& v)
{
    int64_t ans = 0;
    for (size_t i = 0; i < n; ++i) {
        ans += total_energy(p[i], v[i]);
    }
    return ans;
}

std::istream& operator>>(std::istream& in, position& p)
{
    std::string line;
    if (!std::getline(in, line)) {
        return in;
    }
    std::replace_if(
        line.begin(), line.end(),
        [](char c) {
            return c == '<' || c == '>' || c == 'x' || c == 'y' || c == 'z' || c == '=' || c == ',';
        },
        ' ');
    std::istringstream line_in(line);
    if (!(line_in >> p[0] >> p[1] >> p[2])) {
        in.setstate(std::ios::failbit);
    }
    return in;
}

template <size_t n>
void simulate(position_array<n>& p, velocity_array<n>& v)
{
    // Gravity
    for (size_t i = 0; i < n - 1; ++i) {
        for (size_t j = i + 1; j < n; ++j) {
            for (size_t k = 0; k < 3; ++k) {
                if (p[i][k] < p[j][k]) {
                    ++v[i][k];
                    --v[j][k];
                } else if (p[i][k] > p[j][k]) {
                    --v[i][k];
                    ++v[j][k];
                }
            }
        }
    }

    // Velocity
    for (size_t i = 0; i < n; ++i) {
        p[i] += v[i];
    }
}

} // namespace

int main()
{
    constexpr const size_t n = 4;

    position_array<n> p {};
    velocity_array<n> v {};

    for (auto& pi : p) {
        if (!(std::cin >> pi)) {
            return EX_DATAERR;
        }
    }

#if PART == 1
    size_t steps = 1000;
    if (const auto steps_env = getenv("STEPS"); steps_env) {
        steps = std::stol(steps_env);
    }

    for (size_t i = 0; i < steps; ++i) {
        simulate(p, v);
    }

    std::cout << total_energy(p, v) << '\n';
#elif PART == 2
    const auto p0 = p;
    std::optional<size_t> cycle[3];

    for (size_t i = 0; !(cycle[0] && cycle[1] && cycle[2]); ++i) {
        simulate(p, v);

        for (size_t k = 0; k < 3; ++k) {
            if (!cycle[k] && (p[0][k] == p0[0][k] && p[1][k] == p0[1][k] && p[2][k] == p0[2][k] && p[3][k] == p0[3][k] && v[0][k] == 0 && v[1][k] == 0 && v[2][k] == 0 && v[3][k] == 0)) {
                cycle[k] = i + 1;
            }
        }
    }

    std::cout << std::lcm(std::lcm(*cycle[0], *cycle[1]), *cycle[2]) << '\n';
#endif // PART

    return 0;
}
