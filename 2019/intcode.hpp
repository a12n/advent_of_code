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

// Run interrupted until I/O or halt.
std::tuple<opcode, address, address> run_intrpt(state& st, memory& img, address ip);

// Like run_intrpt above, but doesn't preserve state (e.g., relative
// base) between runs.
std::tuple<opcode, address, address> run_intrpt(memory& img, address ip);

// Run uninterrupted until halt, use environment for I/O.
address run(memory& img, address ip, environ& env);
address run(memory& img, address ip = 0);

std::istream& operator>>(std::istream& in, memory& img);

void test(
    const memory& prog,
    address start, address stop,
    const std::forward_list<value>& in, const std::forward_list<value>& out);

// Run interpreter with program from command line parameter or from
// stdin, and I/O attached to stdin and stdout.
int main(int argc, char *argv[]);

} // namespace intcode

#endif  // INTCODE_HPP
