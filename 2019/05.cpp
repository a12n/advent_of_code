#include "advent.hpp"

using intcode::operator>>;

int main()
{
    intcode::memory img;

    if (!(std::cin >> img)) {
        return 1;
    }
    intcode::run(img, 0);

    return 0;
}
