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

//----------------------------------------------------------------------------
// Input and output functions

// Throw exception on input attempts.
inline auto input_error(const std::exception& err = std::runtime_error("unexpected input"))
{
    return [&err]() -> value {
        throw err;
    };
}

// Input characters from the istream in ASCII mode.
inline auto input_stream_ascii(std::istream& s)
{
    return [&s]() {
        return s.get();
    };
}

// Parse values for input from the istream, one value per line.
struct input_stream_line {
    input_stream_line(std::istream& s)
        : s(s)
    {
    }

    intcode::value operator()()
    {
        if (!std::getline(s, line)) {
            throw std::runtime_error(__func__);
        }
        return std::stoll(line);
    };

private:
    std::istream& s;
    std::string line;
};

// Input characters from a string in ASCII mode.
struct input_string_ascii {
    input_string_ascii(std::string_view s)
        : s(s)
    {
    }

    intcode::value operator()()
    {
        if (s.empty()) {
            throw std::length_error(__func__);
        }
        const auto v = s.front();
        s.remove_prefix(1);
        return v;
    }

private:
    std::string_view s;
};

// Always input the same constant value.
inline auto input_value(value v)
{
    return [v]() {
        return v;
    };
}

// Assert next output value equals to n.
inline auto output_assert(value n)
{
    return [n](value v) {
        assert(v == n);
    };
}

// Throws error on output attempts.
inline auto output_error(const std::exception& err = std::runtime_error("unexpected output"))
{
    return [&err](value) {
        throw err;
    };
}

// Output characters to the ostream in ASCII mode.
inline auto output_stream_ascii(std::ostream& s)
{
    return [&s](value v) {
        s.put(v);
    };
}

// Format output values to the ostream, one value per line.
inline auto output_stream_line(std::ostream& s)
{
    return [&s](value v) {
        s << v << '\n';
    };
}

// Append output characters to the provided string in ASCII mode.
inline auto output_string_ascii(std::string& str)
{
    return [&str](value v) {
        str.push_back(v);
    };
}

// Assign output values to the provided reference.
inline auto output_value(value& n)
{
    return [&n](value v) {
        n = v;
    };
}

} // namespace intcode

#endif  // INTCODE_HPP
