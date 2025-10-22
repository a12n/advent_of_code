#include "advent.hpp"

namespace {

using namespace grid::spatial;

struct body {
    position p;
    offset v;
};

int64_t potential_energy(const body& moon)
{
    return taxicab_norm(to_offset(moon.p));
}

int64_t kinetic_energy(const body& moon)
{
    return taxicab_norm(moon.v);
}

int64_t total_energy(const body& moon)
{
    return potential_energy(moon) * kinetic_energy(moon);
}

int64_t total_energy(const std::vector<body>& moons)
{
    int64_t ans = 0;
    for (const auto& moon : moons) {
        ans += total_energy(moon);
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

void simulate(std::vector<body>& moons)
{
    // Gravity
    for (size_t i = 0; i < moons.size() - 1; ++i) {
        for (size_t j = i + 1; j < moons.size(); ++j) {
            for (size_t k = 0; k < 3; ++k) {
                if (moons[i].p[k] < moons[j].p[k]) {
                    ++moons[i].v[k];
                    --moons[j].v[k];
                } else if (moons[i].p[k] > moons[j].p[k]) {
                    --moons[i].v[k];
                    ++moons[j].v[k];
                }
            }
        }
    }

    // Velocity
    for (auto& m : moons) {
        m.p += m.v;
    }
}

} // namespace

int main()
{
    constexpr const size_t n = 4;

    std::vector<body> moons(n, { {}, {} });

    for (auto& moon : moons) {
        if (!(std::cin >> moon.p)) {
            return EX_DATAERR;
        }
    }

    size_t steps = 1000;

    if (const char* steps_env = getenv("STEPS"); steps_env) {
        steps = std::stol(steps_env);
    }

    for (size_t i = 0; i < steps; ++i) {
        simulate(moons);
    }

    std::cout << total_energy(moons) << '\n';

    return 0;
}
