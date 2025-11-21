#include "intcode.hpp"

int main(int argc, char* argv[])
{
    auto img = intcode::load(argc, argv);
    std::string_view script =
#if PART == 1
        /*
        J ABCD
        0 1111
        0 1110
        1 1101
        0 1100
        1 1011
        _ 1010 X
        1 1001
        _ 1000 X
        1 0111
        _ 0110 X
        1 0101
        _ 0100 X
        1 0011
        _ 0010 X
        1 0001
        _ 0000 X

        (!A || !B || !C) && D
        ((!A || !B) || !C) && D
        (!(A && B) || !C) && D
        !((A && B) && C) && D
        !(A && B && C) && D
         */
        "NOT A T\n"
        "NOT T T\n"
        "AND B T\n"
        "AND C T\n"
        "NOT T J\n"
        "AND D J\n"
        "WALK\n"
#elif PART == 2
        // TODO
        "RUN\n"
#endif // PART
        ;
    intcode::value damage = 0;
    intcode::run(img, intcode::input_string_ascii(script), intcode::output_value(damage));
    std::cout << damage << '\n';
    return 0;
}
