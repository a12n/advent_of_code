require('knothash')

local function floodfill(grid, x, y, region)
   local row = grid[y]
   local item = row and row[x]
   if item ~= 0 then
      return
   end
   row[x] = region
   floodfill(grid, x, y - 1, region)
   floodfill(grid, x - 1, y, region)
   floodfill(grid, x + 1, y, region)
   floodfill(grid, x, y + 1, region)
end

local key = io.read()
local grid = {}

for y = 1, 128 do
   local row = {}
   for x, byte in ipairs(densehash(knothash(key .. '-' .. tostring(y - 1)))) do
      for i = 1, 8 do
         if (byte & (1 << (i - 1))) ~= 0 then
            row[(x - 1) * 8 + ((8 - 1) - (i - 1))] = 0
         end
      end
   end
   grid[y] = row
end

local sum = 0
local groups = 0
for y, row in pairs(grid) do
   for x, item in pairs(row) do
      sum = sum + 1
      if item == 0 then
         groups = groups + 1
         floodfill(grid, x, y, groups)
      end
   end
end
print(puzzle.part == 1 and sum or
      puzzle.part == 2 and groups)
