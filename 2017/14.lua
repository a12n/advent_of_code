require('knothash')

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

local key = io.read()
local sum = 0
for i = 1, 128 do
   for _, byte in ipairs(densehash(knothash(key .. '-' .. tostring(i - 1)))) do
      sum = sum + popcount(byte)
   end
end
print(sum)
