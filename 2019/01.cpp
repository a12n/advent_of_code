#include <cstdint>
#include <iostream>

namespace {

using mass = int64_t;

mass fuel_required(mass m)
{
    return m / 3 - 2;
}

mass total_fuel_required(mass m)
{
    mass total_fuel = 0;

    while (true) {
        if ((m = fuel_required(m)) <= 0) {
            break;
        }
        total_fuel += m;
    }

    return total_fuel;
}

} // namespace

int main()
{
    mass m, total_fuel = 0;

    while (std::cin >> m) {
        total_fuel +=
#if PART == 1
            fuel_required(m)
#elif PART == 2
            total_fuel_required(m)
#endif // PART
            ;
    }
    std::cout << total_fuel << '\n';

    return 0;
}
