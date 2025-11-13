#include <array>
#include <iostream>
#include <string>
#include <string_view>

namespace {


const std::array<int, 4> base_pattern = { 0, 1, 0, -1 };

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
inline size_t index(size_t i, size_t j)
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

} // namespace

int main()
{
    std::string signal;

    std::getline(std::cin, signal);

    size_t phases = 100;
    if (const auto phases_env = getenv("PHASES"); phases_env) {
        phases = std::stol(phases_env);
    }

    std::cerr << "signal " << std::quoted(signal) << " phases " << phases << '\n';
    while (phases-- > 0) {
        signal = fft(signal);
    }

    std::cout << signal.substr(0, 8) << '\n';

    return 0;
}
