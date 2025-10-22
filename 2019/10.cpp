#include "advent.hpp"

namespace {

using namespace grid::planar;

using asteroid_list = std::vector<position>;
using asteroid_offset_array = std::vector<offset>;
using asteroid_offset_matrix = std::vector<asteroid_offset_array>;

struct offset_distrib_bucket {
    offset u;
    std::vector<size_t> indices;
};

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

// Count unique normalized vectors from position `i` (for which the
// `asteroid_offset_array` is built) to other positions. Don't count
// the {0, 0} vector to itself.
size_t num_detected_asteroids(const asteroid_offset_array& offsets)
{
    return std::unordered_set(offsets.begin(), offsets.end()).size() - 1;
}

// Vector distribution maps normalized vectors (direction to a
// position from position `i`) to list of the corresponing position
// indices.
std::vector<offset_distrib_bucket> asteroid_offset_distrib(const asteroid_offset_array& offsets)
{
    std::unordered_map<offset, std::vector<size_t>> buckets;
    for (size_t i = 0; i < offsets.size(); ++i) {
        offset zero{};
        if(offsets[i] == zero) {
            continue;
        }
        buckets[offsets[i]].push_back(i);
    }
    std::vector<offset_distrib_bucket> ans;
    for (const auto& [u, indices] : buckets) {
        ans.push_back({ u, indices });
    }
    return ans;
}

// For which `i` row of the offset vector matrix (i.e., for which
// asteroid index `i` in the original asteroid list) there are the
// most unique vectors (i.e., how many other positions are
// visible). If positions `j` and `k` have the same normalized
// direction vector to `i`, then these positions are collinear, and
// only one of them is actually visible from `i`.
std::tuple<size_t, size_t> best_monitoring_location(const asteroid_offset_matrix& offsets)
{
    std::tuple<size_t, size_t> ans {};
    for (size_t i = 0; i < offsets.size(); ++i) {
        const auto num = num_detected_asteroids(offsets[i]);
        if (num > std::get<0>(ans)) {
            ans = { num, i };
        }
    }
    return ans;
}

} // namespace

int main()
{
    asteroid_list asteroids;

    if (!(std::cin >> asteroids).eof()) {
        return EX_DATAERR;
    }

    asteroid_offset_matrix offsets(asteroids.size(), asteroid_offset_array(asteroids.size(), { 0, 0 }));

    for (size_t i = 0; i < asteroids.size() - 1; ++i) {
        for (size_t j = i + 1; j < asteroids.size(); ++j) {
            offsets[i][j] = normalize(asteroids[j] - asteroids[i]);
            offsets[j][i] = -offsets[i][j];
        }
    }

    const auto [optimal_num, optimal_i] = best_monitoring_location(offsets);
#if PART == 1
    std::cout << optimal_num << '\n';
    static_cast<void>(optimal_i);
#elif PART == 2
    static_cast<void>(optimal_num);
    auto distrib = asteroid_offset_distrib(offsets[optimal_i]);
    std::sort(distrib.begin(), distrib.end(), [](const auto& bi, const auto& bj) {
        return angle(bi.u) < angle(bj.u);
    });
    for (auto& [_, indices] : distrib) {
        std::sort(indices.begin(), indices.end(), [&asteroids, optimal_i](size_t i, size_t j) {
            // In reverse order of taxicab_norm. To remove from the back.
            return taxicab_norm(asteroids[optimal_i] - asteroids[i]) > taxicab_norm(asteroids[optimal_i] - asteroids[j]);
        });
    }

    std::cerr << __func__ <<" optimal " << asteroids[optimal_i] << '\n';

    position vaporized;

    for (size_t i = 0;; ++i) {
        const auto i2 = i % distrib.size();

        if (distrib[i2].indices.empty()) {
            continue;
        }

        vaporized = asteroids[distrib[i2].indices.back()];
        std::cerr << __func__ << " vaporized " << (i + 1) << ' ' << vaporized << '\n';
        distrib[i2].indices.pop_back();

        if (i == (200 - 1)) {
            break;
            // TODO
        }
    }
    // TODO: Take 200 points from this distrib. Iterate over different
    // vectors (polar angles) and from each distrib bin take one
    // position (starting from the closest to the optimal_i, as they
    // are sorted by taxicab distance). Stop at position 200 and
    // report it.
#endif // PART

    return 0;
}
