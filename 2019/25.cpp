#include <array>

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

namespace {

template <typename callback_type>
struct event_parser {
    event_parser(callback_type cb)
        : cb(cb)
    {
    }

    void operator()(std::string_view line)
    {
        if (line.empty()) {
            return;
        }
        std::cerr << "line \"" << line << "\"\n";
        (*this.*state)(line);
    }

private:
    void parse_room_name(std::string_view line)
    {
        const auto n = line.size();
        if (n == 0) {
            return;
        }
        if (n < 7 || line.substr(0, 3) != "== " || line.substr(n - 3, 3) != " ==") {
            throw std::invalid_argument(__func__);
        }
        room_name = line.substr(3, n - 3);
        state = &event_parser::parse_doors_header;
    }

    void parse_doors_header(std::string_view line)
    {
        if (line != "Doors here lead:") {
            return;
        }
        state = &event_parser::parse_doors;
    }

    void parse_doors(std::string_view line)
    {
        if (line == "Command?") {
            cb(room_name, doors, items);
            room_name.clear();
            doors = {};
            items.clear();
            state = &event_parser::parse_room_name;
            return;
        }

        if (line == "Items here:") {
            state = &event_parser::parse_items;
            return;
        };

        if (line.size() < 3 || line.substr(0, 2) != "- ") {
            throw std::invalid_argument(__func__);
        }

        if (const auto door = line.substr(2); door == "north") {
            doors[0] = true;
        } else if (door == "west") {
            doors[1] = true;
        } else if (door == "east") {
            doors[2] = true;
        } else if (door == "south") {
            doors[3] = true;
        } else {
            throw std::invalid_argument(__func__);
        }

    }

    void parse_items(std::string_view line)
    {
        if(line=="Command?"){
            cb(room_name,doors,items);
            room_name.clear();
            doors={};
            items.clear();
            state=&event_parser::parse_room_name;
            return;
        }

        if (line.size() < 3 || line.substr(0, 2) != "- ") {
            throw std::invalid_argument(__func__);
        }

        items.emplace_back(line.substr(2));
    }

    void (event_parser::*state)(std::string_view) = &event_parser::parse_room_name;

    std::string room_name {};
    std::array<bool, 4> doors {};
    std::vector<std::string> items {};

    callback_type cb;
};

} // namespace

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
