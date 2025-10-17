#include "advent.hpp"

using namespace grid::planar;

namespace {

using position_time = std::map<position, int64_t>;

position_time intersection(const position_time& s, const position_time& t)
{
    position_time r;
    for (auto i = s.begin(), j = t.begin(); i != s.end() && j != t.end();) {
        if (i->first < j->first) {
            ++i;
        } else if (j->first < i->first) {
            ++j;
        } else {
            r.insert({ i->first, i->second + j->second });
            ++i;
            ++j;
        }
    }
    return r;
}

// Reads "R75 D30 R83 U83 …"
std::istream& operator>>(std::istream& in, position_time& wire)
{
    position p = { 0, 0 };
    int64_t t = 0;
    direction dir;
    int64_t n;

    while (in >> dir >> n) {
        const auto u = to_offset(dir);
        for (; n > 0; --n, ++t, p += u) {
            wire[p] = t;
        }
    }

    return in;
}

} // namespace

int main()
{
    position_time wire[2];

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

    for (const auto& [p, t] : intersection(wire[0], wire[1])) {
        std::cerr << "p " << p << " t " << t << '\n';
#if PART == 1
        if (const auto n = taxicab_norm(to_offset(p)); n != 0 && (!optimal || n < *optimal)) {
            optimal = n;
        }
#elif PART == 2
        if (p != position { 0, 0 } && (!optimal || t < *optimal)) {
            optimal = t;
        }
#endif // PART
    }

    if (!optimal) {
        return 1;
    }

    std::cout << *optimal << '\n';

    return 0;
}
