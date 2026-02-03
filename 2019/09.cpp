#include "intcode.hpp"

int main(int argc, char* argv[])
{
    using namespace intcode;
    auto img = load(argc, argv);
    run(img, input_stream_parse(std::cin), output_stream_format(std::cout));
    return 0;
}
