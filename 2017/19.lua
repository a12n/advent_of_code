require('exts')
require('grid')

local EMPTY = string.byte(' ')
local VERT = string.byte('|')
local HORIZ = string.byte('-')
local TURN = string.byte('+')
local LETTERA = string.byte('A')
local LETTERZ = string.byte('Z')

local diagram = grid.new(io.read('a'))
local pos = point.new(table.find(diagram[1], VERT), 1)
local dir = vector.new(0, 1)
local letters = {}
local steps = 0

while true do
   local item = diagram:get(pos)
   if not item or item == EMPTY then
      break
   elseif item == TURN then
      -- XXX
      local newdir, newpos, newitem
      newdir = dir:ccw()
      newpos = pos + newdir
      newitem = diagram:get(newpos)
      if newitem and newitem ~= EMPTY then
         dir = newdir
         pos = newpos
         steps = steps + 1
      else
         newdir = dir:cw()
         newpos = pos + newdir
         newitem = diagram:get(newpos)
         if newitem and newitem ~= EMPTY then
            dir = newdir
            pos = newpos
            steps = steps + 1
         else
            error('invalid turn')
         end
      end
   elseif item >= LETTERA and item <= LETTERZ then
      table.insert(letters, item)
      pos = pos + dir
      steps = steps + 1
   elseif item == VERT or item == HORIZ then
      pos = pos + dir
      steps = steps + 1
   else
      error('invalid item')
   end
end

if puzzle.part == 1 then
   print(string.char(table.unpack(letters)))
elseif puzzle.part == 2 then
   print(steps)
end
