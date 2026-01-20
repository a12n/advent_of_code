#include <array>
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
    const size_t offset = std::stoul(signal.substr(0, 7));
    std::cerr << "offset " << offset << '\n';
#endif // PART

    return 0;
}
