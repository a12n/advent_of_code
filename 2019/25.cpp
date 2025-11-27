#include "intcode.hpp"

// "molten lava" — "The molten lava is way too hot! You melt!"
// "escape pod" — "You're launched into space! Bye!"
// "giant electromagnet" — "The giant electromagnet is stuck to you.  You can't move!!"
// "photons" — "It is suddenly completely dark! You are eaten by a Grue!"
// "tambourine" — ?
// "polygon" — ?
// "boulder" — ?
// "fuel cell" — ?
// "fixed point" — ?
// "wreath" — ?
// "manifold" — ?

int main(int argc, char* argv[])
{
    auto img = intcode::load(argc, argv);
    std::string_view script = "south\n"
                              "south\n"
                              "take tambourine\n"
                              "north\n"
                              "north\n"
                              // Hull Breach
                              "west\n"
                              "south\n"
                              "take polygon\n"
                              "north\n"
                              "east\n"
                              // Hull Breach
                              "north\n"
                              "west\n"
                              "take boulder\n"
                              "east\n"
                              // Navigation
                              "north\n"
                              "take manifold\n"
                              "north\n"
                              "take hologram\n"
                              "south\n"
                              // Observatory
                              "west\n"
                              "take fuel cell\n"
                              "south\n"
                              "east\n"
                              "south\n"
                              "take fixed point\n"
                              "north\n"
                              "west\n"
                              "north\n"
                              // Holodeck
                              "north\n"
                              "take wreath\n"
                              "east\n"
                              "east\n"
                              "inv\n"
                              "drop boulder\n"
                              "drop fixed point\n"
                              "drop fuel cell\n"
                              "drop hologram\n"
                              "drop manifold\n"
                              "drop polygon\n"
                              "drop tambourine\n"
                              "drop wreath\n"
        // Security Checkpoint
        ;
    intcode::run(
        img,
        intcode::input_string_ascii(script),
        intcode::output_stream_ascii(std::cout));
    return 0;
}
