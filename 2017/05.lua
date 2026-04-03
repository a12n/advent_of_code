local jumps = {}

for line in io.lines() do
   table.insert(jumps, tonumber(line))
end

local strange =
   (puzzle.part == 1 and function(jump)
       return jump + 1
   end) or
   (puzzle.part == 2 and function(jump)
       if jump >= 3 then
          return jump - 1
       else
          return jump + 1
       end
   end)

local ip = 1
local steps = 0
while ip >= 1 and ip <= #jumps do
   ip2 = ip + jumps[ip]
   jumps[ip] = strange(jumps[ip])
   ip = ip2
   steps = steps + 1
end
print(steps)
