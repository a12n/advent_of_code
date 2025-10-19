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

__attribute__((constructor)) void test()
{
#if PART == 2
    {
        // Position mode, input is equal to 8
        const intcode::memory prog { 3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8 };
        intcode::test(prog, 0, 8, { -1 }, { -1 == 8 });
        intcode::test(prog, 0, 8, { 0 }, { 0 == 8 });
        intcode::test(prog, 0, 8, { 7 }, { 7 == 8 });
        intcode::test(prog, 0, 8, { 8 }, { 8 == 8 });
        intcode::test(prog, 0, 8, { 9 }, { 9 == 8 });
    };

    {
        // Immediate mode, input is less than 8
        const intcode::memory prog { 3, 3, 1107, -1, 8, 3, 4, 3, 99 };
        intcode::test(prog, 0, 8, { -1 }, { -1 < 8 });
        intcode::test(prog, 0, 8, { 0 }, { 0 < 8 });
        intcode::test(prog, 0, 8, { 7 }, { 7 < 8 });
        intcode::test(prog, 0, 8, { 8 }, { 8 < 8 });
        intcode::test(prog, 0, 8, { 9 }, { 9 < 8 });
    };
#endif // PART
}
