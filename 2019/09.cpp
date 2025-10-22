#include "intcode.hpp"

int main(int argc, char* argv[])
{
    auto img = intcode::load(argc, argv);
    intcode::run(img);
    return 0;
}
