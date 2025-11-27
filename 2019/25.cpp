#include "intcode.hpp"

// Must not take:
// "escape pod" — "You're launched into space! Bye!"
// "giant electromagnet" — "The giant electromagnet is stuck to you.  You can't move!!"
// "molten lava" — "The molten lava is way too hot! You melt!"
// "photons" — "It is suddenly completely dark! You are eaten by a Grue!"

// May take:
// "boulder"
// "fixed point"
// "fuel cell"
// "hologram"
// "manifold"
// "polygon"
// "tambourine"
// "wreath"

int main(int argc, char* argv[])
{
    auto img = intcode::load(argc, argv);
    intcode::run(img,
        intcode::input_string_ascii(
            // Bring all items to the "Security Checkpoint"
            "south\n"
            "south\n"
            "take tambourine\n"
            "north\n"
            "north\n"
            "west\n"
            "south\n"
            "take polygon\n"
            "north\n"
            "east\n"
            "north\n"
            "west\n"
            "take boulder\n"
            "east\n"
            "north\n"
            "take manifold\n"
            "north\n"
            "take hologram\n"
            "south\n"
            "west\n"
            "take fuel cell\n"
            "south\n"
            "east\n"
            "south\n"
            "take fixed point\n"
            "north\n"
            "west\n"
            "north\n"
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
            "drop wreath\n"),
        intcode::output_stream_ascii(std::cout));
    return 0;
}
