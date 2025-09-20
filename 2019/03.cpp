#include "advent.hpp"

using namespace grid;

namespace {

struct segment {
    direction dir;
    int64_t n;
};

using segment_list = std::vector<segment>;

std::istream& operator>>(std::istream& in, segment& s)
{
    direction d;
    int n;
    if (in >> d >> n) {
        s = { d, n };
    }
    return in;
}

std::istream& operator>>(std::istream& in, segment_list& l)
{
    return ::operator>><segment, ','>(in, l);
}

using point_set = std::set<point>;

point_set intersection(const point_set& s, const point_set& t)
{
    point_set r;
    std::set_intersection(s.begin(), s.end(), t.begin(), t.end(), std::inserter(r, r.begin()));
    return r;
}

} // namespace

int main()
{
    // TODO
    point_set wire[2];

    for (size_t i = 0; i < 2; ++i) {
        std::string line;

        if (!std::getline(std::cin, line)) {
            throw std::runtime_error("getline");
        }

        std::replace(line.begin(), line.end(), ',', ' ');
        std::istringstream in(line);

        point p = { 0, 0 };
        segment s;

        while (in >> s) {
            const auto u = to_vector(s.dir);

            while (s.n--) {
                wire[i].insert(p);
                p = p + u;
            }
        }

        // if (in.fail()) {
        //     throw std::invalid_argument("segment");
        // }
    }

    std::optional<int64_t> optimal_norm {};
    for (const auto& p : intersection(wire[0], wire[1])) {
        const auto n = taxicab_norm(to_vector(p));
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
