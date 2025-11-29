#include <cassert>
#include <cstdlib>
#include <functional>
#include <iostream>
#include <stdexcept>
#include <string>

namespace {

using technique = std::function<size_t(size_t)>;

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
    std::cout << (input<10007>(std::cin))(2019) << '\n';
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
