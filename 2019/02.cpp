#include "advent.hpp"

#if PART == 2
namespace {

std::optional<std::pair<int, int>> gravity_assist(const intcode::memory& prog, int k)
{
    for (int noun = 0; noun < 100; ++noun) {
        for (int verb = 0; verb < 100; ++verb) {
            intcode::memory img = prog;
            img[1] = noun;
            img[2] = verb;
            intcode::run(img, 0);
            if (img[0] == k) {
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
    intcode::memory img;

    if (!(std::cin >> img)) {
        return EX_DATAERR;
    }

#if PART == 1
    img[1] = 12;
    img[2] = 2;
    intcode::run(img, 0);
    std::cout << img[0] << '\n';
#elif PART == 2
    const auto params = gravity_assist(img, 19690720);
    if (!params) {
        return EX_UNAVAILABLE;
    }
    std::cerr << params->first << ' ' << params->second << '\n';
    std::cout << 100 * params->first + params->second << '\n';
#endif // PART

    return 0;
}

int test()
{
#if PART == 1
    intcode::memory img { 1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50 };
    intcode::test_environ env;
    assert(intcode::run(img, 0, env) == 8);
    assert(img[0] == 3500);
#endif // PART
    return 0;
}
