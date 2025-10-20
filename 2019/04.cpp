#include "advent.hpp"

namespace {

using password = std::array<char, 6>;

bool adjacent_digits(password p)
{
#if PART == 1
    for (size_t i = 1; i < p.size(); ++i) {
        if (p[i] == p[i - 1]) {
            return true;
        }
    }
    return false;
#elif PART == 2
    size_t n = 1;
    for (size_t i = 1; i < p.size(); ++i) {
        if (p[i] == p[i - 1]) {
            ++n;
        } else if (n == 2) {
            return true;
        } else {
            n = 1;
        }
    }
    return n == 2;
#endif // PART
}

bool never_decrease(password p)
{
    for (size_t i = 1; i < p.size(); ++i) {
        if (p[i] < p[i - 1]) {
            return false;
        }
    }
    return true;
}

bool valid(password p)
{
    return adjacent_digits(p) && never_decrease(p);
}

password& incr(password& p, size_t i)
{
    if (p[i] < '9') {
        ++p[i];
        return p;
    }
    if (i == 0) {
        throw std::length_error("password");
    }
    p[i] = '0';
    return incr(p, i - 1);
}

password& operator++(password& p)
{
    return incr(p, p.size() - 1);
}

std::istream& operator>>(std::istream& in, password& p)
{
    for (auto& c : p) {
        if (!(in >> c)) {
            break;
        }
        if (c < '0' || c > '9') {
            in.setstate(std::ios::failbit);
            break;
        }
    }
    return in;
}

std::ostream& operator<<(std::ostream& out, password p)
{
    for (auto c : p) {
        out << c;
    }
    return out;
}

} // namespace

int main()
{
    password a, b;
    char c;

    if (!(std::cin >> a >> c >> b) || c != '-') {
        return EX_DATAERR;
    }

    size_t n = 0;
    for (password p = a; p <= b; ++p) {
        n += valid(p);
    }
    std::cout << n << '\n';

    return 0;
}

int test()
{
#if PART == 1
    assert(valid(password { '1', '1', '1', '1', '1', '1' }));
    assert(!never_decrease(password { '2', '2', '3', '4', '5', '0' }));
    assert(!adjacent_digits(password { '1', '2', '3', '7', '8', '9' }));
#elif PART == 2
    assert(valid(password { '1', '1', '2', '2', '3', '3' }));
    assert(!adjacent_digits(password { '1', '2', '3', '4', '4', '4' }));
    assert(valid(password { '1', '1', '1', '1', '2', '2' }));
#endif // PART
    return 0;
}
