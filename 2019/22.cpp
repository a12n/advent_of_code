#include <cassert>
#include <cstdlib>
#include <iostream>
#include <stdexcept>
#include <string>

namespace {

const size_t n_cards = 10007;

template <size_t n>
auto deal_into_new_stack()
{
    return [](size_t i) {
        assert(i < n);
        return (n - 1) - i;
    };
}

template <size_t n>
auto cut_cards(int k)
{
    if (k < 0) {
        k += n;
    }
    assert(k <= n);
    return [k](size_t i) {
        assert(i < n);
        return (i < k) ? (n - k + i) : (i - k);
    };
}

template <size_t n>
auto deal_with_increment(size_t k)
{
    return [k](size_t i) {
        assert(i < n);
        return (i * k) % n;
    };
}

template <typename f_type, typename g_type>
auto operator|(f_type f, g_type g)
{
    return [f, g](size_t i) {
        return g(f(i));
    };
}

} // namespace

int main()
{
    // TODO

    return 0;
}
