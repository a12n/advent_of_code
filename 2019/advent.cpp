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

offset normalize(offset u)
{
    return u /= std::gcd(u[0], u[1]);
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

offset& operator/=(offset& u, int64_t n)
{
    u[0] /= n;
    u[1] /= n;
    return u;
}

offset operator/(offset u, int64_t n)
{
    return u /= n;
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

address src_param_addr(const state& st, const memory& img, mode m, value v)
{
    switch (m) {
    case mode::immediate:
        return v;
    case mode::position:
        return img[v];
    case mode::relative:
        return st.rel_base + img[v];
    default:
        throw std::invalid_argument(__func__);
    }
}

address dest_param_addr(const state& st, const memory& img, mode m, value v)
{
    switch (m) {
    case mode::position:
        return img[v];
    case mode::relative:
        return st.rel_base + img[v];
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

std::tuple<opcode, address, address> run_intrpt(state& st, memory& img, address ip)
{
    const auto err = std::invalid_argument(__func__);

    while (true) {
        const auto instr = img[ip];
        const auto [op, mode1, mode2, mode3] = decode(instr);
        switch (op) {
        case opcode::add:
        case opcode::mul:
        case opcode::less_than:
        case opcode::equals: {
            const auto addr1 = src_param_addr(st, img, mode1, ip + 1);
            const auto addr2 = src_param_addr(st, img, mode2, ip + 2);
            const auto addr3 = dest_param_addr(st, img, mode3, ip + 3);
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
            const auto addr1 = dest_param_addr(st, img, mode1, ip + 1);
            expand(img, addr1);
            ip += 2;
            return { op, ip, addr1 };
        }

        case opcode::output: {
            const auto addr1 = src_param_addr(st, img, mode1, ip + 1);
            expand(img, addr1);
            ip += 2;
            return { op, ip, addr1 };
        }

        case opcode::jump_if_true:
        case opcode::jump_if_false: {
            const auto addr1 = src_param_addr(st, img, mode1, ip + 1);
            const auto addr2 = src_param_addr(st, img, mode2, ip + 2);
            expand(img, std::max(addr1, addr2));
            if ((op == opcode::jump_if_true && img[addr1] != 0) || (op == opcode::jump_if_false && img[addr1] == 0)) {
                ip = img[addr2];
            } else {
                ip += 3;
            }
        } break;

        case opcode::adjust_rel_base: {
            const auto addr1 = src_param_addr(st, img, mode1, ip + 1);
            expand(img, addr1);
            st.rel_base += img[addr1];
            ip += 2;
        } break;

        case opcode::halt:
            return { op, ip, 0 };

        default:
            throw err;
        }
    }
}

std::tuple<opcode, address, address> run_intrpt(memory& img, address ip)
{
    state st;
    return run_intrpt(st, img, ip);
}

address run(memory& img, address ip, environ& env)
{
    state st;
    while (true) {
        const auto [op, ip2, addr] = run_intrpt(st, img, ip);
        switch (op) {
        case opcode::input:
            img[addr] = env.input();
            ip = ip2;
            break;
        case opcode::output:
            env.output(img[addr]);
            ip = ip2;
            break;
        case opcode::halt:
            return ip2;
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

int main(int argc, char* argv[])
{
    memory img;

    if (argc > 1) {
        std::ifstream f(argv[1]);
        if (!(f >> img)) {
            return EX_DATAERR;
        }
    } else {
        if (!(std::cin >> img)) {
            return EX_DATAERR;
        }
    }

    run(img);

    return 0;
}

} // namespace intcode
