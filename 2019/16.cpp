#include <array>
#include <vector>
#include <cassert>
#include <iostream>
#include <string>
#include <string_view>

namespace {


constexpr std::array<int, 4> base_pattern = { 0, 1, 0, -1 };

// Index into base pattern.
//
// i j>0 1 2 3 4 5 6 7 8 9 A B C D E F 0 1
// v______________________________________
// 0 : 0 1 2 3 4 5
// 1 : 0 0 1 1 2 2 3 3 4 4 5 5
// 2 : 0 0 0 1 1 1 2 2 2 3 3 3 4 4 4 5 5 5
//
// j/(i+1)
// (j+1)/(i+1)
constexpr size_t index(size_t i, size_t j)
{
    return ((j + 1) / (i + 1)) % base_pattern.size();
}

std::string fft(std::string_view s)
{
    std::string t(s.size(), '\0');
    for (size_t i = 0; i < s.size(); ++i) {
        int n = 0;
        for (size_t j = 0; j < s.size(); ++j) {
            n += (s[j] - '0') * base_pattern[index(i, j)];
        }
        t[i] = (std::abs(n) % 10) + '0';
    }
    return t;
}

// i=0
//  1  0 -1  0  1  0 -1  0  1  0 -1  0  1  0 -1
// pos1=[0, 4,  8, 12, …]
// neg1=[2, 6, 10, 14, …]=pos1+2
//
// i=1
//  0  1  1  0  0 -1 -1  0  0  1  1  0  0  -1  -1  0  0  1  1  0  0 -1 -1
// pos1=[1, 2,  9, 10, 17, 18, …]
// neg1=[5, 6, 13, 14, 21, 22, …]=pos1+4=pos1+(i+1)*2
//
// i=2
//  0  0  1  1  1  0  0  0 -1 -1 -1  0  0  0  1  1  1  0  0  0 -1 -1 -1  0  0  0  1  1  1  0  0  0 -1 -1 -1
// pos1=[2,3, 4,  14,15,16,  26,27,28, …]
// neg1=[8,9,10,  20,21,22,  32,33,34, …]=pos1+6=pos1+(i+1)*2
//
// i=N
// pos1=first series starts at i, has length i+1, next series starts at +(i+1)*2*2
// neg1=pos1 + (i+1)*2
//
// TODO: Top-down, halve.
void fft(std::string& t, std::string_view s)
{
    t.resize(s.size());

    for (size_t i = 0; i < t.size(); ++i) {
        int x = 0;

        // +1
        for (size_t j = i; j < s.size(); j += (i + 1) * 2 * 2) {
            for (size_t k = 0; k < i + 1; ++k) {
                if (j + k >= s.size()) {
                    break;
                }
                x += (s[j + k] - '0');
            }
        }

        // -1
        for (size_t j = i + (i + 1) * 2; j < s.size(); j += (i + 1) * 2 * 2) {
            for (size_t k = 0; k < i + 1; ++k) {
                if (j + k >= s.size()) {
                    break;
                }
                x -= (s[j + k] - '0');
            }
        }

        t[i] = (std::abs(x) % 10) + '0';
    }
}

//  1  2  3  4  5  6  7  8  9 10
//
//  1  0 -1  0  1  0 -1  0  1  0   = next elt + @1 - @2 - 2@3 + @5 + @6 + @9 - @10
//  0  1  1  0  0 -1 -1  0  0  1   = next elt + @2 - @4 - @5 - @6 - @7 + @10
//  0  0  1  1  1  0  0  0 -1 -1   = next elt + @3 - (row+3) + (row+5) - (row+6)
//  0  0  0  1  1  1  1  0  0  0   = next elt + @4 - (row+4) + (row+5)
//  0  0  0  0  1  1  1  1  1  0   = next elt + @5 - (row+5)
//  0  0  0  0  0  1  1  1  1  1   = next elt + @6
//  0  0  0  0  0  0  1  1  1  1   = next elt + @7
//  0  0  0  0  0  0  0  1  1  1   = next elt + @8
//  0  0  0  0  0  0  0  0  1  1   = next elt + @9
//  0  0  0  0  0  0  0  0  0  1   = last elt
//
//  1  2  3  4  5  6  7  8
//  0  1  2  3  4  5  6  7
//
//  1  0 -1  0  1  0 -1  0
//  0  1  1  0  0 -1 -1  0
//  0  0  1  1  1  0  0  0
//  0  0  0  1  1  1  1  0
//  0  0  0  0  1  1  1  1
//  0  0  0  0  0  1  1  1
//  0  0  0  0  0  0  1  1
//  0  0  0  0  0  0  0  1

// Any N×N sub-matrix starting at top-left element will have it's
// bottom half (sub-matrix of size ⌈N/2⌉×N) of diagonal ones.
//
//     0       N/2       N
//   0 +--------+--------+
//     | recurs |  ? ? ? |
//     |   ive  | ? ? ?  |
// N/2 +--------+--------+
//     | zeroes | diag   |
//     |        |  ones  |
//   N +--------+--------+
void fft2(std::string& t, size_t k, size_t m, std::string_view s, size_t n)
{
    assert(k >= (n / 2));
    assert(k + m <= n);

    t.resize(m);
    for (size_t i = 0; i < m; ++i) {
        int x = 0;

        for (size_t j = i + k; j < n; ++j) {
            std::cerr << __func__ << ": j " << j << '\n';
            x += s[j % s.size()] - '0';
        }

        std::cerr << __func__ << ": i+k " << i+k << '\n';
        t[i] = (std::abs(x) % 10) + '0';
    }
}

} // namespace

int main()
{
    std::string signal, next_signal;

    std::getline(std::cin, signal);

    size_t phases = 100;
    if (const auto phases_env = getenv("PHASES"); phases_env) {
        phases = std::stol(phases_env);
    }

    std::cerr << "signal " << signal << " phases " << phases << '\n';

#if PART == 1
    while (phases-- > 0) {
        fft(next_signal, signal);
        std::swap(signal, next_signal);
    }
    std::cout << signal.substr(0, 8) << '\n';
#elif PART == 2
    // fft2(next_signal, 5, 3, "12345678", 8);

    const size_t offset = std::stoul(signal.substr(0, 7));
    std::cerr << "offset " << offset << '\n';

    const size_t n_full = 10'000 * signal.size();
    const size_t n_part = n_full - offset;
    std::cerr << "n_full " << n_full << '\n'
              << "n_part " << n_part << '\n';

    assert(offset >= (n_full / 2));

    std::vector<int> s, t;

    s.resize(n_part);
    t.resize(n_part);
    for (size_t i = 0; i < n_part; ++i) {
        s[i] = signal[(i + offset) % signal.size()] - '0';
    }

    assert(s[0] == 7);
    assert(s[1] == 4);
    assert(s[2] == 8);
    assert(s[3] == 1);
    assert(s[4] == 1);
    assert(s[5] == 2);
    assert(s[6] == 1);
    assert(s[7] == 2);

    while (phases-- > 0) {
        // std::cerr<<"phases " << phases << '\n';

        t[n_part - 1] = s[n_part - 1];
        for (size_t i = 1; i < n_part; ++i) {
            t[n_part - i - 1] = s[n_part - i - 1] + s[n_part - i];
        }
        for (size_t i = 0; i < n_part; ++i) {
            t[i] = std::abs(t[i]) % 10;
        }

        std::swap(s, t);
    }

    // std::cout << signal.substr(0,8) << '\n';
    for (size_t i = 0; i < 8; ++i) {
        std::cout << ' '<< s[i];
    }
    std::cout << '\n';
#endif // PART

    return 0;
}
