local n = tonumber(os.getenv('N')) or 256

local function reverse(start, len)
   local cond
   if (start + len) > n then
      cond = function(i)
         return i >= start or i < ((start + len) % n)
      end
   else
      cond = function(i)
         return i >= start and i < (start + len)
      end
   end
   return function(i)
      assert(i >= 0 and i < n)
      if cond(i) then
         -- 0 [1 2 3] 4 5
         -- 0 [3 2 1] 4 5
         --
         -- 0 1] 2 3 [4 5 6
         -- 5 4] 2 3 [1 0 6
         --
         -- 4 + (5 - 1) - (_ - 4)
         -- 4 + (5 - 1) - (0 - 4)
         return (start + (len - 1) - (i - start)) % n
      else
         return i
      end
   end
end

local function compose(f, g)
   return function(i)
      return g(f(i))
   end
end

local pos = 0
local skip = 0
local hash = function(i)
   assert(i >= 0 and i < n)
   return i
end

for len in string.gmatch(io.read(), '(%d+)') do
   hash = compose(hash, reverse(pos, len))
   pos = (pos + len + skip) % n
   skip = skip + 1
end

print(hash(0) * hash(1))
