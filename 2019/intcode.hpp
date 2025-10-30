#ifndef INTCODE_HPP
#define INTCODE_HPP

#include <cstdint>
#include <forward_list>
#include <istream>
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

struct state {
    value rel_base {};
};

using memory = std::vector<value>;

// Run interrupted until I/O or halt.
std::tuple<opcode, address> run_intrpt(memory& img, address& ip, value& rel_base);

struct environ {
    virtual value input();
    virtual void output(value v);
};

struct test_environ : environ {
    value input() override;
    void output(value v) override;

    std::forward_list<value> fake_in;
    std::forward_list<value> expected_out;
};

// Run uninterrupted until halt, use environment for I/O.
address run(memory& img);
address run(memory& img, environ& env);

std::istream& operator>>(std::istream& in, memory& img);

void test(const memory& prog, address stop,
    const std::forward_list<value>& in, const std::forward_list<value>& out);

// Load memory image from file or from standard input.
memory load(int argc, char *argv[]);

} // namespace intcode

#endif  // INTCODE_HPP
