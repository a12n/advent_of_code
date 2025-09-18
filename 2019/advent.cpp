#include "advent.hpp"

namespace intcode {

void read(istream& in, memory& p)
{
    string s;
    while (getline(in, s, ',')) {
        p.push_back(stoll(s));
    }
}

address run(memory& p, address ip)
{
    while (true) {
        switch (opcode(p[ip])) {
        case opcode::add:
            p[p[ip + 3]] = p[p[ip + 1]] + p[p[ip + 2]];
            ip += 4;
            break;
        case opcode::mul:
            p[p[ip + 3]] = p[p[ip + 1]] * p[p[ip + 2]];
            ip += 4;
            break;
        case opcode::halt:
            return ip;
        default:
            throw invalid_argument(to_string(p[ip]));
        }
    }
}

} // namespace intcode
