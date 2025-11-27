#include "intcode.hpp"

int main()
{
    using namespace intcode;
    auto img = load(0, nullptr);
    run(img, input_stream_parse(std::cin), output_stream_format(std::cout));
    return 0;
}

int test()
{
    using namespace intcode;
#if PART == 2
    {
        // Position mode, input is equal to 8
        const memory prog { 3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8 };
        assert(8 == run_copy(prog, input_value(-1), output_assert(-1 == 8)));
        assert(8 == run_copy(prog, input_value(0), output_assert(0 == 8)));
        assert(8 == run_copy(prog, input_value(7), output_assert(7 == 8)));
        assert(8 == run_copy(prog, input_value(8), output_assert(8 == 8)));
        assert(8 == run_copy(prog, input_value(9), output_assert(9 == 8)));
    };

    {
        // Immediate mode, input is less than 8
        const memory prog { 3, 3, 1107, -1, 8, 3, 4, 3, 99 };
        assert(8 == run_copy(prog, input_value(-1), output_assert(-1 < 8)));
        assert(8 == run_copy(prog, input_value(0), output_assert(0 < 8)));
        assert(8 == run_copy(prog, input_value(7), output_assert(7 < 8)));
        assert(8 == run_copy(prog, input_value(8), output_assert(8 < 8)));
        assert(8 == run_copy(prog, input_value(9), output_assert(9 < 8)));
    };

    {
        // "…example program uses an input instruction to ask for a
        // single number. The program will then output 999 if the
        // input value is below 8, output 1000 if the input value is
        // equal to 8, or output 1001 if the input value is greater
        // than 8".
        const memory prog {
            3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
            1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
            999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99
        };
        assert(46 == run_copy(prog, input_value(5), output_assert(999))); // 5 < 8
        assert(46 == run_copy(prog, input_value(8), output_assert(1000))); // 8 == 8
        assert(46 == run_copy(prog, input_value(88), output_assert(1001))); // 88 > 8
    };
#endif // PART
    return 0;
}
