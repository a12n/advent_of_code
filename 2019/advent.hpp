#ifndef ADVENT_HPP
#define ADVENT_HPP

#include <cstdint>
#include <functional>
#include <iostream>
#include <optional>
#include <stdexcept>
#include <string>
#include <vector>

using std::cerr;
using std::cin;
using std::cout;
using std::endl;
using std::function;
using std::invalid_argument;
using std::max;
using std::min;
using std::nullopt;
using std::optional;
using std::runtime_error;
using std::string;
using std::vector;

namespace intcode {

using address = size_t;
using value = int64_t;

enum class opcode {
    add = 1,
    mul = 2,
    halt = 99,
};

using memory = vector<value>;

void read(istream& in, memory& p);

address run(memory& p, address ip);

} // namespace intcode

#endif // ADVENT_HPP
