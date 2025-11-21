#ifndef INTCODE_HPP
#define INTCODE_HPP

#include <cassert>
#include <cstdint>
#include <iostream>
#include <stdexcept>
#include <tuple>
#include <vector>

//----------------------------------------------------------------------------
// IntCode Computer

namespace intcode {

using address = size_t;
using value = int64_t;

enum class opcode {
    add = 1,
    mul = 2,
    input = 3,
    output = 4,
    jump_if_true = 5,
    jump_if_false = 6,
    less_than = 7,
    equals = 8,
    adjust_rel_base = 9,
    halt = 99,
};

enum class mode {
    position = 0,
    immediate = 1,
    relative = 2,
};

using memory = std::vector<value>;

// Run interrupted until I/O or halt.
std::tuple<opcode, address> run_intrpt(memory& img, address& ip, value& rel_base);

// Run uninterrupted until halt, use functions for I/O.
template <typename input_func, typename output_func>
address run(memory& img, input_func do_input, output_func do_output)
{
    address ip = 0;
    value rel_base = 0;
    while (true) {
        const auto [op, addr] = run_intrpt(img, ip, rel_base);
        switch (op) {
        case opcode::input:
            img[addr] = do_input();
            break;
        case opcode::output:
            do_output(img[addr]);
            break;
        case opcode::halt:
            return ip;
        default:
            throw std::invalid_argument(__func__);
        }
    }
}

// Run a copy of the provided program until halt, use environment for I/O.
template <typename input_func, typename output_func>
address run_copy(const memory& prog, input_func do_input, output_func do_output)
{
    memory img = prog;
    return run(img, do_input, do_output);
}

std::istream& operator>>(std::istream& in, memory& img);

// Load memory image from file or from standard input.
memory load(int argc, char *argv[]);

} // namespace intcode

#endif  // INTCODE_HPP
