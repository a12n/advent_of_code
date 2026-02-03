#include <cassert>
#include <cstdlib>
#include <functional>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>
#include <variant>

bool starts_with(std::string_view s, std::string_view t)
{
    return s.substr(0, t.size()) == t;
}

std::string_view trim_prefix(std::string_view s, std::string_view t)
{
    if (starts_with(s, t)) {
        s = s.substr(t.size());
    }
    return s;
}

//----------------------------------------------------------------------------

// https://codeforces.com/blog/entry/72527
namespace mod {

template <int64_t m>
constexpr int64_t wrp(int64_t a)
{
    return a < 0 ? a + m : a;
}

template <int64_t m>
constexpr int64_t mod(int64_t a)
{
    return wrp<m>(a % m);
}

template <int64_t m>
constexpr int64_t add(int64_t a, int64_t b)
{
    return mod<m>(a + b);
}

template <int64_t m>
constexpr int64_t sub(int64_t a, int64_t b)
{
    return mod<m>(a - b);
}

template <int64_t m>
constexpr int64_t mul(int64_t a, int64_t b)
{
    __int128_t c = a;
    c *= b;
    c %= m;
    return wrp<m>(c);
}

template <int64_t m>
constexpr int64_t pow(int64_t a, int64_t n)
{
    if (n > 0) {
        if (n % 2 == 0) {
            return pow<m>(mul<m>(a, a), n / 2);
        } else {
            return mul<m>(a, pow<m>(a, n - 1));
        }
    } else if (n == 0) {
        return mod<m>(1);
    } else {
        // XXX
        return 0;
    }
}

// a must be non-zero
// m must be prime
template <int64_t m>
constexpr int64_t inv(int64_t a)
{
    return pow<m>(a, m - 2);
}

template <int64_t m>
constexpr int64_t div(int64_t a, int64_t b)
{
    return mul<m>(a, inv<m>(b));
}

} // namespace mod

//----------------------------------------------------------------------------

// Linear congruential generator `y = (a x + c) % m`.
template <int64_t m>
struct lcg {
    int64_t a = 1, c = 0;

    // f(x) = (a x + c) % m
    constexpr int64_t eval(int64_t x) const
    {
        return mod::add<m>(mod::mul<m>(a, x), c);
    }

    // f^{-1}(x) = ((x - c) / a) % m
    constexpr int64_t eval_inv(int64_t x) const
    {
        return mod::div<m>(mod::sub<m>(x, c), a);
    }
};

// f₁(x) = (a₁ x + c₁) % m
// f₂(x) = (a₂ x + c₂) % m
//
// f₂(f₁(x)) =
// (a₂ (a₁ x + c₁) + c₂) % m =
// ((a₁ a₂) x + (a₂ c₁ + c₂)) % m
template <int64_t m>
constexpr lcg<m> compose(lcg<m> f, lcg<m> g)
{
    return { mod::mul<m>(f.a, g.a), mod::add<m>(mod::mul<m>(g.a, f.c), g.c) };
}

template <int64_t m>
constexpr lcg<m> iterate(lcg<m> f, int64_t n)
{
    if (n > 0) {
        if (n % 2 == 0) {
            return iterate<m>(compose<m>(f, f), n / 2);
        } else {
            return compose<m>(f, iterate<m>(f, n - 1));
        }
    } else if (n == 0) {
        return { 1, 0 };
    } else {
        // XXX
        return { 1 / 0, 0 };
    }
}

//----------------------------------------------------------------------------

namespace shuffle {

template <int64_t m>
constexpr lcg<m> deal_into_new_stack()
{
    return { -1, -1 };
}

template <int64_t m>
constexpr lcg<m> cut_cards(int64_t k)
{
    return { 1, -k };
}

template <int64_t m>
constexpr lcg<m> deal_with_increment(int64_t k)
{
    return { k, 0 };
}

template <int64_t m>
lcg<m> parse(std::string_view s)
{
    if (const auto t = trim_prefix(s, "deal into new stack"); t != s) {
        return deal_into_new_stack<m>();
    } else if (const auto t = trim_prefix(s, "cut "); t != s) {
        return cut_cards<m>(std::stoll(std::string(t)));
    } else if (const auto t = trim_prefix(s, "deal with increment "); t != s) {
        return deal_with_increment<m>(std::stoll(std::string(t)));
    } else {
        throw std::invalid_argument(__func__);
    }
}

template <int64_t m>
lcg<m> input(std::istream& s)
{
    lcg<m> f;
    std::string l;

    while (std::getline(s, l)) {
        f = compose<m>(f, parse<m>(l));
    }

    return f;
}

} // namespace shuffle

//----------------------------------------------------------------------------

int main()
{
    std::cout <<
#if PART == 1
        shuffle::input<10007>(std::cin).eval(2019)
#elif PART == 2
        iterate(shuffle::input<119315717514047>(std::cin), 101741582076661).eval_inv(2020)
#endif // PART
              << '\n';
    return 0;
}
