#include "advent.hpp"

using intcode::operator>>;

int main()
{
    intcode::memory p;

    if (!(std::cin >> p)) {
        return 1;
    }

#if PART == 1
    intcode::run(p, 0);
#endif // PART

    return 0;
}
