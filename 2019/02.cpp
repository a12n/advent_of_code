#include "advent.hpp"

#if PART == 2
namespace {

std::optional<std::pair<int, int>> gravity_assist(const intcode::memory& p, int k)
{
    for (int noun = 0; noun < 100; ++noun) {
        for (int verb = 0; verb < 100; ++verb) {
            intcode::memory t = p;
            t[1] = noun;
            t[2] = verb;
            intcode::run(t, 0);
            if (t[0] == k) {
                return { { noun, verb } };
            }
        }
    }
    return {};
}

} // namespace
#endif // PART

int main()
{
    intcode::memory p;

    if (!(std::cin >> p)) {
        return 1;
    }

#if PART == 1
    p[1] = 12;
    p[2] = 2;
    intcode::run(p, 0);
    std::cout << p[0] << '\n';
#elif PART == 2
    const auto params = gravity_assist(p, 19690720);
    if (!params) {
        return 1;
    }
    std::cerr << params->first << ' ' << params->second << '\n';
    std::cout << 100 * params->first + params->second << '\n';
#endif // PART

    return 0;
}
