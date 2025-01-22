module Day01

const CCW = -1 ::Int
const CW  = +1 ::Int

function rotation(c::Char)
    c == 'L' && return CCW
    c == 'R' && return CW
    throw(ArgumentError("invalid rotation"))
end

instruction(s) = (rotation(s[1]), parse(Int, s[2:end]))

const ROTATE = [ 0 -1
                 1  0] ::Matrix{Int}

# Taxicab distance from P to origin.
distance(p::Vector{Int}) = abs(p[1]) + abs(p[2])

function part1(input::IO, output::IO)
    dir = [0; -1]
    pos = [0; 0]

    while (str = strip(readuntil(input, ','))) != ""
        cw, n = instruction(str)

        dir = cw * ROTATE * dir
        pos = pos + n * dir
    end

    println(output, distance(pos))
end

function part2(input::IO, output::IO)
    dir = [0; -1]
    pos = [0; 0]

    visited = Set{Vector{Int}}()
    push!(visited, pos)

    while (str = strip(readuntil(input, ','))) != ""
        cw, n = instruction(str)
        dir = cw * ROTATE * dir

        for i in 1:n
            if (pos += dir) in visited
                return println(output, distance(pos))
            end
            push!(visited, pos)
        end
    end

    throw(ErrorException("no solution"))
end

end
