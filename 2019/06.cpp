#include <iostream>
#include <string>
#include <unordered_map>

#include <sysexits.h>

namespace {

// orbits_around["A"] = "B" means that object A orbits around object B.
using orbits_map = std::unordered_map<std::string, std::string>;

std::istream& operator>>(std::istream& in, orbits_map& orbits)
{
    std::string object, parent;
    orbits.clear();
    while (std::getline(in, parent, ')') && std::getline(in, object)) {
        orbits.insert({ object, parent });
    }
    return in;
}

// The depth of the node in the orbits_map tree. Number of objects
// directly and indirectly orbited by the specified object. The input
// is small enough for this straightforward implementation to be good
// enough.
size_t depth(const orbits_map& orbits, const std::string& object)
{
    if (const auto parent = orbits.find(object); parent != orbits.end()) {
        return 1 + depth(orbits, parent->second);
    }
    return 0;
}

size_t total_orbits(const orbits_map& orbits)
{
    size_t ans = 0;
    for (const auto& [object, _] : orbits) {
        ans += depth(orbits, object);
    };
    return ans;
}

std::string lca(const orbits_map& orbits, std::string obj1, std::string obj2)
{
    size_t depth1 = depth(orbits, obj1);
    size_t depth2 = depth(orbits, obj2);
    while (depth1 != depth2) {
        if (depth1 > depth2) {
            obj1 = orbits.at(obj1);
            --depth1;
        } else {
            obj2 = orbits.at(obj2);
            --depth2;
        }
    }
    while (obj1 != obj2) {
        obj1 = orbits.at(obj1);
        obj2 = orbits.at(obj2);
    }
    return obj1;
}

size_t num_orbit_transfers(const orbits_map& orbits, std::string a, std::string b)
{
    return
        // The path from root to A…
        depth(orbits, a)
        // …to the object orbited by A, actually.
        - 1
        // The same for B.
        + depth(orbits, b) - 1
        // Less the path from root to LCA twice (for both A and B).
        - 2 * depth(orbits, lca(orbits, a, b));
}

} // namespace

int main()
{
    orbits_map orbits;

    if (!(std::cin >> orbits).eof()) {
        return EX_DATAERR;
    }

#if PART == 1
    std::cout << total_orbits(orbits) << '\n';
#elif PART == 2
    std::cout << num_orbit_transfers(orbits, "YOU", "SAN") << '\n';
#endif // PART

    return 0;
}
