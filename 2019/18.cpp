#include <cassert>
#include <iostream>
#include <map>
#include <string>
#include <vector>

#include "input.hpp"

namespace {

using distance_map = std::map<char, std::map<char, size_t>>;
using vault_map = std::vector<std::string>;

distance_map distances(const vault_map& vault)
{
    // TODO
    return {};
}

} // namespace

template <>
vault_map input<vault_map>(std::istream& s)
{
    vault_map vault;
    while (true) {
        vault.emplace_back();
        if (!std::getline(s, vault.back())) {
            if (!s.eof()) {
                throw std::invalid_argument(__func__);
            }
            vault.pop_back();
            return vault;
        }
        assert(vault.back().size() == vault.front().size());
    }
}

int main()
{
    const auto dists = distances(input<vault_map>(std::cin));

    // TODO

    return 0;
}
