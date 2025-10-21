#include "advent.hpp"

using namespace grid::planar;

using asteroid_list = std::vector<position>;

std::istream& operator>>(std::istream& in, asteroid_list& asteroids)
{
    asteroids.clear();
    for (int64_t y = 0;; ++y) {
        for (int64_t x = 0;; ++x) {
            const char c = in.get();
            if (c == '#' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
                asteroids.push_back({ x, y });
            } else if (c == '.') {
                // OK
            } else if (c == '\n') {
                break;
            } else {
                in.setstate(std::ios::failbit);
                return in;
            }
        }
    }
}

int main()
{
    asteroid_list asteroids;

    if (!(std::cin >> asteroids).eof()) {
        return EX_DATAERR;
    }

    std::optional<size_t> num_detected;

    for (size_t i = 0; i < asteroids.size(); ++i) {
        std::unordered_set<offset> offsets;

        for (size_t j = 0; j < asteroids.size(); ++j) {
            if (j != i) {
                offsets.insert(normalize(asteroids[j] - asteroids[i]));
            }
        }

        if (!num_detected || offsets.size() > *num_detected) {
            num_detected = offsets.size();
        }
    }

    if (!num_detected) {
        return EX_UNAVAILABLE;
    }

    std::cout << *num_detected << '\n';

    return 0;
}
