#include "intcode.hpp"

int main(int argc, char* argv[])
{
    intcode::run_copy(
        intcode::load(argc, argv),
        intcode::input_stream_ascii(std::cin),
        intcode::output_stream_ascii(std::cout));
    return 0;
}
