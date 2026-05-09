require('knothash')

local key = io.read()

if puzzle.part == 1 then
   local function popcount(k)
      local n = 0
      while k ~= 0 do
         if (k & 1) ~= 0 then
            n = n + 1
         end
         k = k >> 1
      end
      return n
   end

   local sum = 0

   for i = 1, 128 do
      for _, byte in ipairs(densehash(knothash(key .. '-' .. tostring(i - 1)))) do
         sum = sum + popcount(byte)
      end
   end

   print(sum)
elseif puzzle.part == 2 then
   local grid = {}

   for rowi = 1, 128 do
      local row = {}
      for coli, byte in ipairs(densehash(knothash(key .. '-' .. tostring(rowi - 1)))) do
         for biti = 1, 8 do
            if (byte & (1 << (biti - 1))) ~= 0 then
               row[(coli - 1) * 8 + (biti - 1) + 1] = true
            end
         end
      end
      grid[rowi] = row
   end

   -- TODO
   for rowi = 1, 128 do
      for coli = 1, 128 do
         if grid[rowi][coli] then
            sum = sum + 1
            io.stderr:write('#')
         else
            io.stderr:write('.')
         end
      end
      io.stderr:write('\n')
   end
end
