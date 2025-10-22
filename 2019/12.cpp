#include "advent.hpp"

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
#elif PART == 2
    size_t steps = -1;
    const auto p0 = p;
    const auto v0 = v;
    std::optional<size_t> pn {}, vn {};
#endif // PART

    for (size_t i = 0; i < steps; ++i) {
        simulate(p, v);
#if PART == 2
        if (p == p0 && !pn) {
            pn = i;
            std::cerr << "pn " << *pn << '\n';
        }
        if (v == v0 && !vn) {
            vn = i;
            std::cerr << "vn " << *vn << '\n';
        }
        if (pn && vn) {
            break;
        }
#endif // PART
    }

#if PART == 1
    std::cout << total_energy(p, v) << '\n';
#elif PART == 2
    std::cout << std::lcm(*pn, *vn) << '\n';
#endif // PART

    return 0;
}
