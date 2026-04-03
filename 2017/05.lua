local jumps = {}

for line in io.lines() do
   table.insert(jumps, tonumber(line))
end

local ip = 1
local steps = 0
while ip >= 1 and ip <= #jumps do
   ip2 = ip + jumps[ip]
   jumps[ip] = jumps[ip] + 1
   ip = ip2
   steps = steps + 1
end
print(steps)
