-- https://www.redblobgames.com/grids/hexagons/

require('grid-spatial')

local direction = {
   n  = vector.new( 0, -1,  1),
   nw = vector.new(-1,  0,  1),
   ne = vector.new( 1, -1,  0),
   sw = vector.new(-1,  1,  0),
   se = vector.new( 1,  0, -1),
   s  = vector.new( 0,  1, -1),
}

local offset = vector.new()
local latest = nil
local furthest = nil

for line in io.lines() do
   for dir in string.gmatch(line, '(%a+),?') do
      offset = offset + direction[dir]
      latest = offset:taxicab() // 2
      if not furthest or latest > furthest then
         furthest = latest
      end
   end
end

if puzzle.part == 1 then
   print(latest)
elseif puzzle.part == 2 then
   print(furthest)
end
