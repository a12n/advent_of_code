#include "intcode.hpp"

int main(int argc, char* argv[])
{
    using namespace intcode;
    memory img = load(argc, argv);
    run(img, input_stream_ascii(std::cin), output_stream_ascii(std::cerr));
    return 0;
}
