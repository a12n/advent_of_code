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

} // namespace

int main()
{
    // TODO
    position_set wire[2];

    for (size_t i = 0; i < 2; ++i) {
        std::string line;

        if (!std::getline(std::cin, line)) {
            throw std::runtime_error("getline");
        }

        std::replace(line.begin(), line.end(), ',', ' ');
        std::istringstream in(line);

        position p = { 0, 0 };
        direction dir;
        int64_t n;

        while (in >> dir >> n) {
            const auto u = to_offset(dir);
            for (; n > 0; --n, p += u) {
                wire[i].insert(p);
            }
        }

        // if (in.fail()) {
        //     throw std::invalid_argument("segment");
        // }
    }

    std::optional<int64_t> optimal_norm {};
    for (const auto& p : intersection(wire[0], wire[1])) {
        const auto n = taxicab_norm(to_offset(p));
        if (n == 0) {
            continue;
        }
        if (!optimal_norm || n < *optimal_norm) {
            optimal_norm = n;
        }
    }

    if (!optimal_norm) {
        return 1;
    }

    std::cout << *optimal_norm << '\n';

    return 0;
}
