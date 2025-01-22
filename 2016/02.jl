module Day02

const DOWN  = 1
const LEFT  = 2
const RIGHT = 3
const UP    = 4

function direction(c::Char)
    c == 'D' && return DOWN
    c == 'L' && return LEFT
    c == 'R' && return RIGHT
    c == 'U' && return UP
    throw(ArgumentError("invalid direction"))
end

function bathroomcode(input::IO, output::IO, adjacent)
    button = '5'
    for line in eachline(input)
        for char in line
            button = adjacent(button)[direction(char)]
        end
        print(output, button)
    end
    println(output)
end

function part1(input::IO, output::IO)
    bathroomcode(
        input, output,
        (button::Char) -> begin
            button == '1' && return "4121"
            button == '2' && return "5132"
            button == '3' && return "6233"
            button == '4' && return "7451"
            button == '5' && return "8462"
            button == '6' && return "9563"
            button == '7' && return "7784"
            button == '8' && return "8795"
            button == '9' && return "9896"
            throw(ArgumentError("invalid button"))
        end
    )
end

function part2(input::IO, output::IO)
    bathroomcode(
        input, output,
        (button::Char) -> begin
            button == '1' && return "3111"
            button == '2' && return "6232"
            button == '3' && return "7241"
            button == '4' && return "8344"
            button == '5' && return "5565"
            button == '6' && return "A572"
            button == '7' && return "B683"
            button == '8' && return "C794"
            button == '9' && return "9899"
            button == 'A' && return "AAB6"
            button == 'B' && return "DAC7"
            button == 'C' && return "CBC8"
            button == 'D' && return "DDDB"
            throw(ArgumentError("invalid button"))
        end
    )
end

end
