#include "advent.hpp"

using namespace grid::planar;

namespace {

using position_set = std::set<position>;

position_set intersection(const position_set& s, const position_set& t)
{
    position_set r;
    std::set_intersection(s.begin(), s.end(), t.begin(), t.end(), std::inserter(r, r.begin()));
    return r;
}

// Reads "R75 D30 R83 U83 …"
std::istream& operator>>(std::istream& in, position_set& wire)
{
    position p = { 0, 0 };
    direction dir;
    int64_t n;

    while (in >> dir >> n) {
        const auto u = to_offset(dir);
        for (; n > 0; --n, p += u) {
            wire.insert(p);
        }
    }

    return in;
}

} // namespace

int main()
{
    position_set wire[2];

    for (size_t i = 0; i < 2; ++i) {
        std::string line;

        if (!std::getline(std::cin, line)) {
            throw std::runtime_error("getline");
        }

        std::replace(line.begin(), line.end(), ',', ' ');
        std::istringstream in(line);

        if (!(in >> wire[i]).eof()) {
            throw std::invalid_argument(line);
        }
    }

    std::optional<int64_t> optimal {};

    for (const auto& p : intersection(wire[0], wire[1])) {
        const auto n = taxicab_norm(to_offset(p));

        if (n == 0) {
            continue;
        }
        if (!optimal || n < *optimal) {
            optimal = n;
        }
    }

    if (!optimal) {
        return 1;
    }

    std::cout << *optimal << '\n';

    return 0;
}
