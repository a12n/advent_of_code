#ifndef INPUT_HPP
#define INPUT_HPP

#include <istream>
#include <optional>
#include <stdexcept>

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

#endif  // INPUT_HPP
