require('grid')

local CLEAN = string.byte('.')
local WEAKENED = string.byte('W')
local INFECTED = string.byte('#')
local FLAGGED = string.byte('F')

local function unevolved(cluster, pos, dir)
   local infected = false
   local node = cluster:get(pos) or CLEAN
   if node == INFECTED then
      dir = dir:cw()
      cluster:set(pos, CLEAN)
   elseif node == CLEAN then
      dir = dir:ccw()
      cluster:set(pos, INFECTED)
      infected = true
   else
      error('invalid node')
   end
   return pos + dir, dir, infected
end

local function evolved(cluster, pos, dir)
   local infected = false
   local node = cluster:get(pos) or CLEAN
   if node == CLEAN then
      dir = dir:ccw()
      cluster:set(pos, WEAKENED)
   elseif node == WEAKENED then
      infected = true
      cluster:set(pos, INFECTED)
   elseif node == INFECTED then
      dir = dir:cw()
      cluster:set(pos, FLAGGED)
   elseif node == FLAGGED then
      dir = dir:cw():cw()
      cluster:set(pos, CLEAN)
   else
      error('invalid node')
   end
   return pos + dir, dir, infected
end

local cluster = grid.new(io.read('a'))
local pos = point.new(cluster:ncols() // 2 + 1, cluster:nrows() // 2 + 1)
local dir = vector.new(0, -1)
local infections = 0
local virus = (puzzle.part == 1 and unevolved or
               puzzle.part == 2 and evolved)

for burst = 1, (puzzle.part == 1 and 10000 or
                puzzle.part == 2 and 10000000) do
   local infected
   pos, dir, infected = virus(cluster, pos, dir)
   if infected then
      infections = infections + 1
   end
end
print(infections)
