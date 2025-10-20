#include "advent.hpp"

namespace {

// The number of amplifiers.
constexpr const size_t n = 5;

intcode::value amplifier_controller(
    const intcode::memory& prog, std::array<intcode::value, n> phase)
{
    // Memory and instruction pointers for all N amplfiers.
    std::array<intcode::memory, n> img;
    std::array<intcode::address, n> ip;

    // Index of the currently running amplifier and bit-field to track
    // running state of all amplifiers.
    size_t amp = 0;
    size_t running = 0;

    // Load the program into the amplifiers.
    for (size_t i = 0; i < n; ++i) {
        img[i] = prog;
        ip[i] = 0;
        running |= (1 << i);
    }

    intcode::value signal = 0;

    // While not all amplifiers halt.
    while (running != 0) {
        const auto [op, ip2, addr] = intcode::run_intrpt(img[amp], ip[amp]);
        switch (op) {
        case intcode::opcode::input:
            if (phase[amp] == -1) {
                // The amplifier is already configured with the phase
                // setting. Provide it with the signal from the
                // previous stage.
                img[amp][addr] = signal;
            } else {
                // Initial configuration of the amplifier.
                img[amp][addr] = phase[amp];
                phase[amp] = -1;
            }
            ip[amp] = ip2;
            break;
        case intcode::opcode::output:
            // Amplifier iteration finished. Save the returned signal
            // for the next stage.
            signal = img[amp][addr];
            ip[amp] = ip2;
            // Start running the next amplification stage.
            amp = (amp + 1) % n;
            break;
        case intcode::opcode::halt:
            running &= ~(1 << amp);
            ip[amp] = ip2;
            amp = (amp + 1) % n;
            break;
        default:
            assert(false);
        }
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
               { 3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23,
                   101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99, 0, 0 },
               { 0, 1, 2, 3, 4 })
        == 54321);
    assert(amplifier_controller(
               { 3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33,
                   1002, 33, 7, 33, 1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0 },
               { 1, 0, 4, 3, 2 })
        == 65210);
#elif PART == 2
    assert(amplifier_controller(
               { 3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26,
                   27, 4, 27, 1001, 28, -1, 28, 1005, 28, 6, 99, 0, 0, 5 },
               { 9, 8, 7, 6, 5 })
        == 139629729);
    assert(amplifier_controller(
               { 3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001, 54,
                   -5, 54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53, 55, 53, 4,
                   53, 1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10 },
               { 9, 7, 8, 5, 6 })
        == 18216);
#endif // PART
}
