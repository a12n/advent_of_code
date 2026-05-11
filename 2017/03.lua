require('grid')

local pos = point.new(0, 0)
local dir = vector.new(1, 0)
local len = 1

if puzzle.part == 1 then
   local index = tonumber(io.read()) - 1
   while index > 0 do
      local m

      m = math.min(len, index)
      pos = pos + dir * m
      dir = dir:ccw()
      index = index - m

      m = math.min(len, index)
      pos = pos + dir * m
      dir = dir:ccw()
      index = index - m

      len = len + 1
   end
   print(vector.taxicab(pos))
end
