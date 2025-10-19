#include "advent.hpp"

namespace grid::planar {

direction opposite(direction dir)
{
    switch (dir) {
    case direction::up:
        return direction::down;
    case direction::left:
        return direction::right;
    case direction::right:
        return direction::left;
    case direction::down:
        return direction::up;
    default:
        throw std::invalid_argument(__func__);
    }
}

int64_t taxicab_norm(offset u)
{
    return std::abs(u[0]) + std::abs(u[1]);
}

direction to_direction(char c)
{
    switch (c) {
    case 'U':
    case 'u':
        return direction::up;
    case 'L':
    case 'l':
        return direction::left;
    case 'R':
    case 'r':
        return direction::right;
    case 'D':
    case 'd':
        return direction::down;
    default:
        throw std::invalid_argument(__func__);
    }
}

position to_position(offset u)
{
    return { u[0], u[1] };
}

offset to_offset(direction d)
{
    switch (d) {
    case direction::up:
        return { 0, 1 };
    case direction::left:
        return { -1, 0 };
    case direction::right:
        return { 1, 0 };
    case direction::down:
        return { 0, -1 };
    default:
        throw std::invalid_argument(__func__);
    }
}

offset to_offset(position p)
{
    return { p[0], p[1] };
}

offset& operator*=(offset& u, int64_t n)
{
    u[0] *= n;
    u[1] *= n;
    return u;
}

offset operator*(int64_t n, offset u)
{
    return u *= n;
}

position& operator+=(position& p, offset u)
{
    p[0] += u[0];
    p[1] += u[1];
    return p;
}

position operator+(position p, offset u)
{
    return p += u;
}

offset& operator+=(offset& u, offset v)
{
    u[0] += v[0];
    u[1] += v[1];
    return u;
}

offset operator+(offset u, offset v)
{
    return u += v;
}

offset operator-(position p, position q)
{
    return { p[0] - q[0], p[1] - q[1] };
}

position& operator-=(position& p, offset u)
{
    p[0] -= u[0];
    p[1] -= u[1];
    return p;
}

position operator-(position p, offset u)
{
    return p -= u;
}

offset operator-(offset v)
{
    return { -v[0], -v[1] };
}

std::istream& operator>>(std::istream& in, direction& dir)
{
    char c;
    if (in >> c) {
        try {
            dir = to_direction(c);
        } catch (std::invalid_argument&) {
            in.setstate(std::ios::failbit);
        }
    }
    return in;
}

std::ostream& operator<<(std::ostream& out, offset u)
{
    return out << '[' << u[0] << ',' << u[1] << ']';
}

std::ostream& operator<<(std::ostream& out, position p)
{
    return out << to_offset(p);
}

} // namespace grid::planar

namespace intcode {

std::tuple<opcode, mode, mode, mode> decode(value v)
{
    return {
        opcode(v % 100),
        mode((v / 100) % 10),
        mode((v / 1000) % 10),
        mode((v / 10000) % 10)
    };
}

value environ::input()
{
    std::string s;
    std::cerr << '>' << '\n';
    if (!std::getline(std::cin, s)) {
        throw std::runtime_error(__func__);
    }
    return std::stoll(s);
}

void environ::output(value v)
{
    std::cerr << '<';
    std::cout << v << '\n';
}

address run(memory& m, address ip)
{
    environ empty;
    return run(m, ip, empty);
}

address run(memory& m, address ip, environ& env)
{
    const auto err = std::invalid_argument(__func__);

    while (true) {
        const auto [op, mode1, mode2, mode3] = decode(m[ip]);
        switch (op) {
        case opcode::add:
        case opcode::mul: {
            const auto addr1 = (mode1 == mode::immediate ? ip + 1 : mode1 == mode::position ? m[ip + 1]
                                                                                            : throw err);
            const auto addr2 = (mode2 == mode::immediate ? ip + 2 : mode2 == mode::position ? m[ip + 2]
                                                                                            : throw err);
            const auto addr3 = (mode3 == mode::position ? m[ip + 3] : throw err);
            if (op == opcode::add) {
                m[addr3] = m[addr1] + m[addr2];
            } else {
                m[addr3] = m[addr1] * m[addr2];
            }
            ip += 4;
        } break;

        case opcode::input: {
            const auto addr1 = (mode1 == mode::position ? m[ip + 1] : throw err);
            m[addr1] = env.input();
            ip += 2;
        } break;

        case opcode::output: {
            const auto addr1 = (mode1 == mode::immediate ? ip + 1 : mode1 == mode::position ? m[ip + 1]
                                                                                            : throw err);
            env.output(m[addr1]);
            ip += 2;
        } break;

        case opcode::jump_if_true:
        case opcode::jump_if_false: {
            const auto addr1 = (mode1 == mode::immediate ? ip + 1 : mode1 == mode::position ? m[ip + 1]
                                                                                            : throw err);
            const auto addr2 = (mode1 == mode::immediate ? ip + 2 : mode2 == mode::position ? m[ip + 2]
                                                                                            : throw err);
            if ((op == opcode::jump_if_true && m[addr1] != 0) || (op == opcode::jump_if_false && m[addr1] == 0)) {
                ip = m[addr2];
            } else {
                ip += 3;
            }
        } break;

        case opcode::halt:
            return ip;

        default:
            throw err;
        }
    }
}

std::istream& operator>>(std::istream& in, memory& m)
{
    return ::operator>><int64_t, ','>(in, m);
}

} // namespace intcode
