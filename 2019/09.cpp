#include "intcode.hpp"

int main(int argc, char* argv[])
{
    using namespace intcode;
    auto img = load(argc, argv);
    run(img, input_stream_line(std::cin), output_stream_line(std::cout));
    return 0;
}
