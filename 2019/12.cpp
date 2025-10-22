#include "advent.hpp"

namespace {

using namespace grid::spatial;

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
int64_t total_energy(const std::array<position, n>& p, const std::array<offset, n>& v)
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
void simulate(std::array<position, n>& p, std::array<offset, n>& v)
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

    std::array<position, n> p {};
    std::array<offset, n> v {};

    for (auto& pi : p) {
        if (!(std::cin >> pi)) {
            return EX_DATAERR;
        }
    }

    size_t steps = 1000;

    if (const char* steps_env = getenv("STEPS"); steps_env) {
        steps = std::stol(steps_env);
    }

    for (size_t i = 0; i < steps; ++i) {
        simulate(p, v);
    }

    std::cout << total_energy(p, v) << '\n';

    return 0;
}
