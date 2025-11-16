#ifndef INPUT_HPP
#define INPUT_HPP

#include <istream>
#include <optional>
#include <ostream>
#include <stdexcept>
#include <type_traits>

template <typename value_type>
std::optional<value_type> input(std::istream& s)
{
    value_type v;
    if (s >> v)
        return v;
    if (s.eof())
        return {};
    throw std::invalid_argument(__func__);
}

template <typename value_type>
void output(std::ostream& s, std::conditional_t<std::is_scalar_v<value_type>, value_type, const value_type&> v)
{
    s << v;
}

#endif  // INPUT_HPP
