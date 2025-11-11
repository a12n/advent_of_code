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

template <typename t, size_t n>
std::ostream& operator<<(std::ostream& out, const std::array<t, n>& p)
{
    out << '[';
    for (size_t i = 0; i < n; ++i) {
        if (i != 0) {
            out << ' ';
        }
        out << p[i];
    }
    return out << ']';
}

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
#elif PART == 2
    size_t steps = -1;
    std::map<std::array<int64_t, 2 * n>, size_t> xn;
    std::map<std::array<int64_t, 2 * n>, size_t> yn;
    std::map<std::array<int64_t, 2 * n>, size_t> zn;
#endif // PART

    for (size_t i = 0; i < steps; ++i) {
#if PART == 2
        std::cerr << i << " p " << p << " v " << v << '\n';

        const std::array<int64_t, 2 * n> xk = { p[0][0], p[1][0], p[2][0], p[3][0], v[0][0], v[1][0], v[2][0], v[3][0] },
                                         yk = { p[0][1], p[1][1], p[2][1], p[3][1], v[0][1], v[1][1], v[2][1], v[3][1] },
                                         zk = { p[0][2], p[1][2], p[2][2], p[3][2], v[0][2], v[1][2], v[2][2], v[3][2] };

        const auto xi = xn.find(xk);
        const auto yi = yn.find(yk);
        const auto zi = zn.find(zk);

        if (xi != xn.end()) {
            std::cerr << i << " ==x " << xi->second << '\n';
        }
        if (yi != yn.end()) {
            std::cerr << i << " ==y " << yi->second << '\n';
        }
        if (zi != zn.end()) {
            std::cerr << i << " ==z " << zi->second << '\n';
        }

        xn.insert({ xk, i });
        yn.insert({ yk, i });
        zn.insert({ zk, i });
#endif // PART
        simulate(p, v);
    }

#if PART == 1
    std::cout << total_energy(p, v) << '\n';
#elif PART == 2
    // std::cout << std::lcm(*pn, *vn) << '\n';
#endif // PART

    return 0;
}
