#include "intcode.hpp"

int main(int argc, char* argv[])
{
    intcode::ascii_environ env;
    intcode::memory img = intcode::load(argc, argv);
    run(img, env);
    return 0;
}
