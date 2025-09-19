#include "advent.hpp"

namespace {

using mass_t = int64_t;

mass_t fuel_required(mass_t mass)
{
    return mass / 3 - 2;
}

mass_t total_fuel_required(mass_t mass)
{
    mass_t total_fuel = 0;

    while (true) {
        if ((mass = fuel_required(mass)) <= 0) {
            break;
        }
        total_fuel += mass;
    }

    return total_fuel;
}

} // namespace

int main()
{
    mass_t mass, total_fuel = 0;

    while (std::cin >> mass) {
        total_fuel +=
#if PART == 1
            fuel_required(mass)
#elif PART == 2
            total_fuel_required(mass)
#endif // PART
            ;
    }
    std::cout << total_fuel << '\n';

    return 0;
}
