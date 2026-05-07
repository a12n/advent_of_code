local a = tonumber(string.match(io.read(), 'A starts with (%d+)'))
local b = tonumber(string.match(io.read(), 'B starts with (%d+)'))

function lcg(k, n)
   return (k * n) % 2147483647
end

function lcga(n)
   return lcg(16807, n)
end

function lcgb(n)
   return lcg(48271, n)
end

local n = 0
if puzzle.part == 1 then
   for i = 1, 40000000 do
      if (a & 0xFFFF) == (b & 0xFFFF) then
         n = n + 1
      end
      a = lcga(a)
      b = lcgb(b)
   end
elseif puzzle.part == 2 then
   local coa = coroutine.create(function()
         while true do
            if a % 4 == 0 then
               coroutine.yield(a)
            end
            a = lcga(a)
         end
   end)
   local cob = coroutine.create(function()
         while true do
            if b % 8 == 0 then
               coroutine.yield(b)
            end
            b = lcgb(b)
         end
   end)
   for i = 1, 5000000 do
      local _, aval = coroutine.resume(coa)
      local _, bval = coroutine.resume(cob)
      if (aval & 0xFFFF) == (bval & 0xFFFF) then
         n = n + 1
      end
   end
   coroutine.close(coa)
   coroutine.close(cob)
end
print(n)
