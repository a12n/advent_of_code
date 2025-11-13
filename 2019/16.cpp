#include <iostream>
#include <string>

int main()
{
    std::string signal;
    std::getline(std::cin, signal);
    std::cerr << "signal '" << signal << "'\n";
    return 0;
}
