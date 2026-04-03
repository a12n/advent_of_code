require('grid')

local index = tonumber(io.read()) - 1
local pos = point.new(0, 0)
local dir = vector.new(1, 0)

local n = 1
while index > 0 do
   local m

   m = math.min(n, index)
   pos = pos + dir * m
   dir = dir:ccw()
   index = index - m

   m = math.min(n, index)
   pos = pos + dir * m
   dir = dir:ccw()
   index = index - m

   n = n + 1
end

print(vector.taxicab(pos))
