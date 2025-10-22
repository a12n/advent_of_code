#include <array>
#include <cstdint>
#include <iostream>
#include <optional>

namespace {

enum color : uint8_t {
    black = 0,
    white = 1,
    transparent = 2,
};

constexpr const std::array<char, 3> rendering = { '#', '.', '_' };

color& operator<<=(color& a, color b)
{
    if (a == transparent) {
        a = b;
    }
    return a;
}

constexpr const size_t n_rows = 6, n_cols = 25;

using image_row = std::array<color, n_cols>;
using image_layer = std::array<image_row, n_rows>;
using color_freqs = std::array<size_t, 3>;

std::istream& operator>>(std::istream& in, image_layer& layer)
{
    for (auto& row : layer) {
        for (auto& pixel : row) {
            char c;
            if (!(in >> c)) {
                return in;
            }
            switch (c) {
            case '0':
            case rendering[black]:
                pixel = black;
                break;
            case '1':
            case rendering[white]:
                pixel = white;
                break;
            case '2':
            case rendering[transparent]:
                pixel = transparent;
                break;
            default:
                in.setstate(std::ios::failbit);
                return in;
            }
        }
    }
    return in;
}

// XXX: The image appears in different format than in operator>>
std::ostream& operator<<(std::ostream& out, const image_layer& layer)
{
    for (const auto& row : layer) {
        for (const auto pixel : row) {
            out << rendering[pixel];
        }
        out << '\n';
    }
    return out;
}

color_freqs frequencies(const image_layer& layer)
{
    color_freqs ans {};
    for (const auto& row : layer) {
        for (const auto pixel : row) {
            ++ans[pixel];
        }
    }
    return ans;
}

image_layer& operator<<=(image_layer& layer, const image_layer& underlay)
{
    for (size_t row = 0; row < n_rows; ++row) {
        for (size_t col = 0; col < n_cols; ++col) {
            layer[row][col] <<= underlay[row][col];
        }
    }
    return layer;
}

} // namespace

int main()
{
    image_layer layer;

#if PART == 1
    std::optional<color_freqs> optimal_freqs;
#elif PART == 2
    std::optional<image_layer> visible;
#endif // PART

    while (std::cin >> layer) {
#if PART == 1
        const color_freqs freqs = frequencies(layer);
        if (!optimal_freqs || freqs[0] < (*optimal_freqs)[0]) {
            optimal_freqs = freqs;
        }
#elif PART == 2
        if (visible) {
            *visible <<= layer;
        } else {
            visible = layer;
        }
#endif // PART
    }

#if PART == 1
    std::cout << (*optimal_freqs)[1] * (*optimal_freqs)[2] << '\n';
#elif PART == 2
    std::cout << (*visible);
#endif // PART

    return 0;
}
