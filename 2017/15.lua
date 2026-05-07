local function input(name)
   return tonumber(string.match(io.read(), 'Generator ' .. name .. ' starts with (%d+)'))
end

local function lcg(k, x)
   return (k * x) % 2147483647
end

local function lcga(x)
   return lcg(16807, x)
end

local function lcgb(x)
   return lcg(48271, x)
end

local function judge(n, a, b)
   return (a & 0xFFFF) == (b & 0xFFFF) and n + 1 or n
end

local a = input('A')
local b = input('B')
local n = 0

if puzzle.part == 1 then
   for i = 1, 40000000 do
      n = judge(n, a, b)
      a = lcga(a)
      b = lcgb(b)
   end
elseif puzzle.part == 2 then
   for i = 1, 5000000 do
      while a % 4 ~= 0 do
         a = lcga(a)
      end
      while b % 8 ~= 0 do
         b = lcgb(b)
      end
      n = judge(n, a, b)
      a = lcga(a)
      b = lcgb(b)
   end
end

print(n)
