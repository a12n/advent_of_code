#include "intcode.hpp"

int main()
{
    return intcode::main(0, nullptr);
}

int test()
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

    {
        // "…example program uses an input instruction to ask for a
        // single number. The program will then output 999 if the
        // input value is below 8, output 1000 if the input value is
        // equal to 8, or output 1001 if the input value is greater
        // than 8".
        const intcode::memory prog {
            3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
            1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
            999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99
        };
        intcode::test(prog, 0, 46, { 5 }, { 999 }); // 5 < 8
        intcode::test(prog, 0, 46, { 8 }, { 1000 }); // 8 == 8
        intcode::test(prog, 0, 46, { 88 }, { 1001 }); // 88 > 8
    };
#endif // PART
    return 0;
}
