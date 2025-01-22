module Day03

triangle(s) = map((t) -> parse(Int, t), split(s, isspace, keepempty=false))

valid(a, b, c) = (b + c) > a && (a + c) > b && (a + b) > c

function part1(input::IO, output::IO)
    n = 0
    for line in eachline(input)
        n += valid(triangle(line)...)
    end
    println(output, n)
end

function part2(input::IO, output::IO)
    n = 0
    while !eof(stdin)
        a1, a2, a3 = triangle(readline(stdin))
        b1, b2, b3 = triangle(readline(stdin))
        c1, c2, c3 = triangle(readline(stdin))
        n += valid(a1, b1, c1)
        n += valid(a2, b2, c2)
        n += valid(a3, b3, c3)
    end
    println(output, n)
end

end
