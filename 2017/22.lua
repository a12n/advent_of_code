require('grid')

local CLEAN = string.byte('.')
local INFECTED = string.byte('#')

local cluster = grid.new(io.read('a'))

local pos = point.new(cluster:ncols() // 2 + 1, cluster:nrows() // 2 + 1)
local dir = vector.new(0, -1)
local infections = 0
for burst = 1, 10000 do
   local node = cluster:get(pos) or CLEAN
   -- print('burst', burst)
   -- print('pos', pos, 'dir', dir)
   -- print('node', string.char(node))
   if node == INFECTED then
      dir = dir:cw()
      cluster:set(pos, CLEAN)
      -- print('dir<-', dir)
      -- print('node<-', string.char(CLEAN))
   elseif node == CLEAN then
      dir = dir:ccw()
      cluster:set(pos, INFECTED)
      infections = infections + 1
      -- print('dir<-', dir)
      -- print('node<-', string.char(INFECTED))
      -- print('infections<-', infections)
   else
      error('invalid node')
   end
   pos = pos + dir
   -- print()
end
print(infections)
