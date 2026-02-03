#include <cassert>
#include <cstdlib>
#include <functional>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>
#include <variant>

namespace alt {

template <size_t n>
struct deal_into_new_stack {
    size_t operator()(size_t i) const
    {
        assert(i < n);
        return (n - 1) - i;
    }
};

template <size_t n>
struct cut_cards {
    cut_cards() = default;

    cut_cards(int init_k)
        : k(init_k < 0 ? init_k + n : init_k)
    {
        assert(k <= n);
    }

    cut_cards(int init_k, bool inv)
        : cut_cards(inv ? -init_k : init_k)
    {
    }

    size_t operator()(size_t i) const
    {
        assert(i < n);
        return (i < k) ? (n - k + i) : (i - k);
    }

    const size_t k {};
};

template <size_t n>
struct deal_with_increment {
    deal_with_increment() = default;

    deal_with_increment(size_t k, bool inv = false)
        : k(k)
        , inv(inv)
    {
    }

    size_t operator()(size_t i) const
    {
        assert(i < n);
        // k = 3
        // 0 1 2 3 4 5 6 7 8 9 ->
        // 0 7 4 1 8 5 2 9 6 3
        // *   . *     *     *
        //
        // k = 7
        // 0 1 2 3 4 5 6 7 8 9 ->
        // 0 3 6 9 2 5 8 1 4 7
        // *       .     *
        if (inv) {
            // TODO: Extended Euclidean algorithm?
            while (i % k != 0) {
                i += n;
            }
            return i / k;
        } else {
            return (i * k) % n;
        }
    }

    const size_t k {};
    const bool inv {};
};

template <size_t n>
struct technique {
    std::variant<deal_into_new_stack<n>, cut_cards<n>, deal_with_increment<n>> f;

    size_t operator()(size_t i) const
    {
        return std::visit([i](auto&& v) -> size_t { return v(i); }, f);
    }
};

template <size_t n>
struct technique_list {
    std::vector<technique<n>> fs;

    size_t operator()(size_t i) const
    {
        for (const auto& f : fs) {
            i = f(i);
        }
        return i;
    }
};

template <size_t n>
technique_list<n> optimize(const technique_list<n>& techniques)
{
    technique_list<n> ans;

    // TODO
    // deal_into_new_stack | deal_with_increment k = ?
    // deal_into_new_stack | cut_cards k = ?
    // deal_with_increment k1 | cut_cards k2 = ?
    // cut_cards k1 | deal_with_increment k2 = ?

    return ans;
}

} // namespace alt

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

namespace {

using technique = std::function<size_t(size_t)>;
using technique_ptr = std::shared_ptr<technique>;

template <size_t n>
technique deal_into_new_stack()
{
    return [](size_t i) {
        assert(i < n);
        return (n - 1) - i;
    };
}

template <size_t n>
technique cut_cards(int k, bool inv = false)
{
    if (inv) {
        k = -k;
    }
    if (k < 0) {
        k += n;
    }
    assert(static_cast<size_t>(k) <= n);
    return [k](size_t i) {
        assert(i < n);
        return (i < static_cast<size_t>(k)) ? (n - k + i) : (i - k);
    };
}

template <size_t n>
technique deal_with_increment(size_t k, bool inv = false)
{
    // k = 3
    // 0 1 2 3 4 5 6 7 8 9 ->
    // 0 7 4 1 8 5 2 9 6 3
    // *   . *     *     *
    //
    // k = 7
    // 0 1 2 3 4 5 6 7 8 9 ->
    // 0 3 6 9 2 5 8 1 4 7
    // *       .     *
    if (inv) {
        return [k](size_t i) {
            // TODO: No loop?
            while (i % k != 0) {
                i += n;
            }
            return i / k;
        };
    } else {
        return [k](size_t i) {
            assert(i < n);
            return (i * k) % n;
        };
    }
}

technique operator|(technique f, technique g)
{
    return [f, g](size_t i) {
        return g(f(i));
    };
}

// There may be large trees of techique application with some techique
// G used over and over again across the tree. In this case, techique
// G should be allocated once and only the pointer should be used
// instead of newly constructed function.
technique operator|(technique_ptr f, technique_ptr g)
{
    return [f, g](size_t i) {
        return (*g)((*f)(i));
    };
}

template <size_t n>
technique input(std::istream& s, bool inv = false, technique f = [](size_t i) { return i; })
{
    std::string tok;

    // FIXME
    if (s >> tok) {
        if (tok == "deal") {
            if (!(s >> tok)) {
                throw std::invalid_argument(__func__);
            }
            if (tok == "into") {
                if (!(s >> tok) || tok != "new" || !(s >> tok) || tok != "stack") {
                    throw std::invalid_argument(__func__);
                }
                const auto g = deal_into_new_stack<n>();
                return input<n>(s, inv, inv ? (g | f) : (f | g));
            } else if (tok == "with") {
                size_t k;
                if (!(s >> tok) || tok != "increment" || !(s >> k)) {
                    throw std::invalid_argument(__func__);
                }
                const auto g = deal_with_increment<n>(k, inv);
                return input<n>(s, inv, inv ? (g | f) : (f | g));
            }
        } else if (tok == "cut") {
            int k;
            if (!(s >> k)) {
                throw std::invalid_argument(__func__);
            }
            const auto g = cut_cards<n>(k, inv);
            return input<n>(s, inv, inv ? (g | f) : (f | g));
        } else {
            throw std::invalid_argument(__func__);
        }
    }

    if (s.eof()) {
        return f;
    }

    throw std::invalid_argument(__func__);
}

} // namespace

int main()
{
#if PART == 1
    const auto shuffle = input<10007>(std::cin);
    std::cout << shuffle(2019) << '\n';
#elif PART == 2
    // Technique F performed N times by "exponentiation by squaring" principle.
    //
    // Let F_N is the technique F performed N times.
    // F_1 = F
    // F_2 = F_1 | F_1
    // F_4 = F_2 | F_2
    // F_8 = F_4 | F_4
    // …
    // F_N = F_{N / 2} | F_{N / 2} for any N what is power of 2.
    //
    // Any positive N may be represented as sum of powers of 2.
    // E.g.
    // N = 5 = 4 + 1 = 0b101
    // F_5 = F_4 | F_1
    //
    // The `shuffle_inv` array, stores at index I the techique applied
    // 2**I times for I up to (64 - 1). This then may be used to apply
    // the techique any number of times up to 2**64.
    std::array<technique_ptr, 64> shuffle_inv;

    // Technique performed 2**0 = 1 times.
    shuffle_inv[0] = std::make_shared<technique>(input<119315717514047>(std::cin, true));
    for (size_t i = 1; i < shuffle_inv.size(); ++i) {
        // Technique performed 2**i times.
        shuffle_inv[i] = std::make_shared<technique>(shuffle_inv[i - 1] | shuffle_inv[i - 1]);
    }

    // Perform technique N = 101741582076661 number of times. For each
    // power of two component 2**k of N, perform technique 2**k times.
    size_t i = 2020;
    for (size_t n = 101741582076661, k = 0; n != 0; n >>= 1, ++k) {
        std::cerr << "n " << n
                  << " k " << k
                  << " i " << i
                  << '\n';

        if (n & 1) {
            // TODO: Memoization.
            i = (*shuffle_inv[k])(i);
        }
    }

    std::cout << i << '\n';
#endif // PART
    return 0;
}

int test()
{
    const size_t n = 10;

#if PART == 1
    {
        const auto shuffle = deal_with_increment<n>(7)
            | deal_into_new_stack<n>()
            | deal_into_new_stack<n>();
        assert(shuffle(0) == 0);
        assert(shuffle(1) == 7);
        assert(shuffle(2) == 4);
        assert(shuffle(3) == 1);
        assert(shuffle(4) == 8);
        assert(shuffle(5) == 5);
        assert(shuffle(6) == 2);
        assert(shuffle(7) == 9);
        assert(shuffle(8) == 6);
        assert(shuffle(9) == 3);
    }

    {
        const auto shuffle = cut_cards<n>(6)
            | deal_with_increment<n>(7)
            | deal_into_new_stack<n>();
        assert(shuffle(0) == 1);
        assert(shuffle(1) == 4);
        assert(shuffle(2) == 7);
        assert(shuffle(3) == 0);
        assert(shuffle(4) == 3);
        assert(shuffle(5) == 6);
        assert(shuffle(6) == 9);
        assert(shuffle(7) == 2);
        assert(shuffle(8) == 5);
        assert(shuffle(9) == 8);
    }

    {
        const auto shuffle = deal_with_increment<n>(7)
            | deal_with_increment<n>(9)
            | cut_cards<n>(-2);
        assert(shuffle(0) == 2);
        assert(shuffle(1) == 5);
        assert(shuffle(2) == 8);
        assert(shuffle(3) == 1);
        assert(shuffle(4) == 4);
        assert(shuffle(5) == 7);
        assert(shuffle(6) == 0);
        assert(shuffle(7) == 3);
        assert(shuffle(8) == 6);
        assert(shuffle(9) == 9);
    }

    {
        const auto shuffle = deal_into_new_stack<n>()
            | cut_cards<n>(-2)
            | deal_with_increment<n>(7)
            | cut_cards<n>(8)
            | cut_cards<n>(-4)
            | deal_with_increment<n>(7)
            | cut_cards<n>(3)
            | deal_with_increment<n>(9)
            | deal_with_increment<n>(3)
            | cut_cards<n>(-1);
        assert(shuffle(0) == 7);
        assert(shuffle(1) == 4);
        assert(shuffle(2) == 1);
        assert(shuffle(3) == 8);
        assert(shuffle(4) == 5);
        assert(shuffle(5) == 2);
        assert(shuffle(6) == 9);
        assert(shuffle(7) == 6);
        assert(shuffle(8) == 3);
        assert(shuffle(9) == 0);
    }
#elif PART == 2
    {
        const auto unshuffle = deal_into_new_stack<n>()
            | deal_into_new_stack<n>()
            | deal_with_increment<n>(7, true);

        assert(unshuffle(0) == 0);
        assert(unshuffle(1) == 3);
        assert(unshuffle(2) == 6);
        assert(unshuffle(3) == 9);
        assert(unshuffle(4) == 2);
        assert(unshuffle(5) == 5);
        assert(unshuffle(6) == 8);
        assert(unshuffle(7) == 1);
        assert(unshuffle(8) == 4);
        assert(unshuffle(9) == 7);
    }

    {
        const auto unshuffle = deal_into_new_stack<n>()
            | deal_with_increment<n>(7, true)
            | cut_cards<n>(6, true);

        assert(unshuffle(0) == 3);
        assert(unshuffle(1) == 0);
        assert(unshuffle(2) == 7);
        assert(unshuffle(3) == 4);
        assert(unshuffle(4) == 1);
        assert(unshuffle(5) == 8);
        assert(unshuffle(6) == 5);
        assert(unshuffle(7) == 2);
        assert(unshuffle(8) == 9);
        assert(unshuffle(9) == 6);
    }
#endif // PART

    return 0;
}
