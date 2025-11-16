#include <cassert>
#include <iostream>
#include <map>
#include <string>
#include <vector>

#include <sysexits.h>

namespace {

using distance_map = std::map<char, std::map<char, size_t>>;
using vault_map = std::vector<std::string>;

std::istream& operator>>(std::istream& in, vault_map& vault)
{
    vault.clear();
    while (true) {
        vault.emplace_back();
        if (!std::getline(in, vault.back())) {
            vault.pop_back();
            break;
        }
        assert(vault.back().size() == vault.front().size());
    }
    return in;
}

} // namespace

int main()
{
    vault_map vault;

    if (!(std::cin >> vault).eof()) {
        return EX_DATAERR;
    }

    return 0;
}
