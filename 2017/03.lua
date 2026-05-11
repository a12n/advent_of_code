require('exts')
require('grid')

local pos = point.new(0, 0)
local dir = vector.new(1, 0)
local len = 1

if puzzle.part == 1 then
   local index = tonumber(io.read()) - 1
   while index > 0 do
      for _ = 1, 2 do
         local m = math.min(len, index)
         pos = pos + dir * m
         dir = dir:ccw()
         index = index - m
      end
      len = len + 1
   end
   print(vector.taxicab(pos))
elseif puzzle.part == 2 then
   local value = tonumber(io.read())
   local memory = grid.new()
   memory:set(pos, 1)
   while true do
      for _ = 1, 2 do
         for _ = 1, len do
            pos = pos + dir
            local nsum = math.sum(table.unpack(memory:neighbors(pos, 8, 0)))
            memory:set(pos, nsum)
            if nsum > value then
               print(nsum)
               return
            end
         end
         dir = dir:ccw()
      end
      len = len + 1
   end
end
