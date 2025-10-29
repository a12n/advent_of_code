#include <algorithm>
#include <cassert>
#include <fstream>
#include <iostream>
#include <stdexcept>

#include "intcode.hpp"

namespace {

//----------------------------------------------------------------------------
// I/O Utilities

template <typename item_type, char sep = ','>
std::istream& operator>>(std::istream& in, std::vector<item_type>& items)
{
    items.clear();

    item_type v;

    if (!(in >> v)) {
        return in;
    }
    items.push_back(std::move(v));

    while (true) {
        char c;

        if (!(in >> c)) {
            if (in.eof()) {
                in.clear(in.rdstate() & ~std::ios::failbit);
            }
            break;
        }

        if (c != sep) {
            in.unget();
            break;
        }

        if (!(in >> v)) {
            in.setstate(std::ios::failbit);
            break;
        }
        items.push_back(std::move(v));
    }

    return in;
}

} // namespace

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

value test_environ::input()
{
    assert(!fake_in.empty());
    value v = fake_in.front();
    fake_in.pop_front();
    return v;
}

void test_environ::output(value v)
{
    assert(v == expected_out.front());
    expected_out.pop_front();
}

address src_param_addr(const memory& img, value rel_base, mode m, value v)
{
    switch (m) {
    case mode::immediate:
        return v;
    case mode::position:
        return img[v];
    case mode::relative:
        return rel_base + img[v];
    default:
        throw std::invalid_argument(__func__);
    }
}

address dest_param_addr(const memory& img, value rel_base, mode m, value v)
{
    switch (m) {
    case mode::position:
        return img[v];
    case mode::relative:
        return rel_base + img[v];
    default:
        throw std::invalid_argument(__func__);
    }
}

void expand(memory& img, address addr)
{
    if (addr >= img.size()) {
        img.resize(addr + 1, 0);
    }
}

std::tuple<opcode, address> run_intrpt(memory& img, address& ip, value& rel_base)
{
    while (true) {
        const auto instr = img[ip];
        const auto [op, mode1, mode2, mode3] = decode(instr);
        switch (op) {
        case opcode::add:
        case opcode::mul:
        case opcode::less_than:
        case opcode::equals: {
            const auto addr1 = src_param_addr(img, rel_base, mode1, ip + 1);
            const auto addr2 = src_param_addr(img, rel_base, mode2, ip + 2);
            const auto addr3 = dest_param_addr(img, rel_base, mode3, ip + 3);
            expand(img, std::max({ addr1, addr2, addr3 }));
            if (op == opcode::add) {
                img[addr3] = img[addr1] + img[addr2];
            } else if (op == opcode::mul) {
                img[addr3] = img[addr1] * img[addr2];
            } else if (op == opcode::less_than) {
                img[addr3] = (img[addr1] < img[addr2]);
            } else if (op == opcode::equals) {
                img[addr3] = (img[addr1] == img[addr2]);
            }
            ip += 4;
        } break;

        case opcode::input: {
            const auto addr1 = dest_param_addr(img, rel_base, mode1, ip + 1);
            expand(img, addr1);
            ip += 2;
            return { op, addr1 };
        }

        case opcode::output: {
            const auto addr1 = src_param_addr(img, rel_base, mode1, ip + 1);
            expand(img, addr1);
            ip += 2;
            return { op, addr1 };
        }

        case opcode::jump_if_true:
        case opcode::jump_if_false: {
            const auto addr1 = src_param_addr(img, rel_base, mode1, ip + 1);
            const auto addr2 = src_param_addr(img, rel_base, mode2, ip + 2);
            expand(img, std::max(addr1, addr2));
            if ((op == opcode::jump_if_true && img[addr1] != 0) || (op == opcode::jump_if_false && img[addr1] == 0)) {
                ip = img[addr2];
            } else {
                ip += 3;
            }
        } break;

        case opcode::adjust_rel_base: {
            const auto addr1 = src_param_addr(img, rel_base, mode1, ip + 1);
            expand(img, addr1);
            rel_base += img[addr1];
            ip += 2;
        } break;

        case opcode::halt:
            return { op, 0 };

        default:
            throw std::invalid_argument(__func__);
        }
    }
}

address run(memory& img, environ& env)
{
    address ip = 0;
    value rel_base = 0;
    while (true) {
        const auto [op, addr] = run_intrpt(img, ip, rel_base);
        switch (op) {
        case opcode::input:
            img[addr] = env.input();
            break;
        case opcode::output:
            env.output(img[addr]);
            break;
        case opcode::halt:
            return ip;
        default:
            throw std::invalid_argument(__func__);
        }
    }
}

address run(memory& img, address ip)
{
    environ def;
    return run(img, ip, def);
}

std::istream& operator>>(std::istream& in, memory& img)
{
    return ::operator>><int64_t, ','>(in, img);
}

void test(
    const memory& prog,
    address start, address stop,
    const std::forward_list<value>& in, const std::forward_list<value>& out)
{
    memory img = prog;
    test_environ env;
    env.fake_in = in;
    env.expected_out = out;
    assert(run(img, start, env) == stop);
    assert(env.fake_in.empty());
    assert(env.expected_out.empty());
}

memory load(int argc, char* argv[])
{
    memory img;
    if (argc > 1) {
        std::ifstream f(argv[1]);
        if (!(f >> img)) {
            throw std::invalid_argument(__func__);
        }
    } else {
        if (!(std::cin >> img)) {
            throw std::invalid_argument(__func__);
        }
    }
    return img;
}

} // namespace intcode
