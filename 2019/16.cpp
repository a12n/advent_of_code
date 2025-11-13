#include <iostream>
#include <string>

namespace {

std::string fft(const std::string& s)
{
    // TODO
    return s;
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

    std::cerr << "signal '" << signal << "' phases " << phases << '\n';
    while (phases-- > 0) {
        signal=fft(signal);
    }

    std::cerr << signal.substr(0, 8) << '\n';

    return 0;
}
