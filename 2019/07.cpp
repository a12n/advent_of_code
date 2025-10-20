#include "advent.hpp"

namespace {

intcode::value amplifier_controller(
    const intcode::memory& prog, const std::array<intcode::value, 5>& phase)
{
    intcode::pipe_environ env;
    intcode::value signal = 0;

    for (size_t i = 0; i < phase.size(); ++i) {
        intcode::memory img = prog;
        env.output(phase[i]);
        env.output(signal);
        intcode::run(img, 0, env);
        signal = env.input();
    }

    return signal;
}

} // namespace

int main()
{
    intcode::memory img;

    if (!(std::cin >> img)) {
        return EX_DATAERR;
    }

    intcode::value max_signal = 0;

    // FIXME
    for (int a = 0; a < 5; ++a) {
        for (int b = 0; b < 5; ++b) {
            if (b == a) {
                continue;
            }
            for (int c = 0; c < 5; ++c) {
                if (c == a || c == b) {
                    continue;
                }
                for (int d = 0; d < 5; ++d) {
                    if (d == a || d == b || d == c) {
                        continue;
                    }
                    for (int e = 0; e < 5; ++e) {
                        if (e == a || e == b || e == c || e == d) {
                            continue;
                        }
                        max_signal = std::max(max_signal, amplifier_controller(img, { a, b, c, d, e }));
                    }
                }
            }
        }
    }

    std::cout << max_signal << '\n';

    return 0;
}

__attribute__((constructor)) void test()
{
#if PART == 1
    assert(amplifier_controller(
               { 3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0 },
               { 4, 3, 2, 1, 0 })
        == 43210);
    assert(amplifier_controller(
               { 3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23, 101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99, 0, 0 },
               { 0, 1, 2, 3, 4 })
        == 54321);
    assert(amplifier_controller(
               { 3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33, 1002, 33, 7, 33, 1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0 },
               { 1, 0, 4, 3, 2 })
        == 65210);
#endif // PART
}
