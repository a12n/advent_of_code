local function reverse(from, len, n)
   local to = from + len
   local tomod = to % n
   if to <= n then
      -- from=2 len=2 n=5
      --      f    t
      -- 0 1 (2 3) 4
      -- 0 1 (3 2) 4
      return function(i)
         assert(i >= 0 and i < n)
         if i >= from and i < to then
            return (from + (len - 1) - (i - from)) % n
         end
         return i
      end
   else
      -- from=3 len=4 n=5 to=7 tomod=2
      --      t  f
      -- 0 1) 2 (3 4
      -- 4 3) 2 (1 0
      return function(i)
         assert(i >= 0 and i < n)
         if i >= from then
            return ((tomod - 1) - (i - from)) % n
         elseif i < tomod then
            return (from + (tomod - 1) - i) % n
         end
         return i
      end
   end
end

local function compose(f, g)
   return function(i)
      return g(f(i))
   end
end

if puzzle.test then
   local hash
   local n = 5

   -- (0 1 2) 3 4
   -- (2 1 0) 3 4
   hash = reverse(0, 3, n)
   assert(hash(0) == 2)
   assert(hash(1) == 1)
   assert(hash(2) == 0)
   assert(hash(3) == 3)
   assert(hash(4) == 4)

   -- 0 1 (2 3) 4
   -- 0 1 (3 2) 4
   hash = reverse(2, 2, n)
   assert(hash(0) == 0)
   assert(hash(1) == 1)
   assert(hash(2) == 3)
   assert(hash(3) == 2)
   assert(hash(4) == 4)

   -- (0 1 2 3 4)
   -- (4 3 2 1 0)
   hash = reverse(0, 5, n)
   assert(hash(0) == 4)
   assert(hash(1) == 3)
   assert(hash(2) == 2)
   assert(hash(3) == 1)
   assert(hash(4) == 0)

   -- 0 1) 2 (3 4
   -- 4 3) 2 (1 0
   hash = reverse(3, 4, n)
   assert(hash(0) == 4)
   assert(hash(1) == 3)
   assert(hash(2) == 2)
   assert(hash(3) == 1)
   assert(hash(4) == 0)

   -- 0 1 2) (3 4
   -- 0 4 3) (2 1
   hash = reverse(3, 5, n)
   assert(hash(0) == 0)
   assert(hash(1) == 4)
   assert(hash(2) == 3)
   assert(hash(3) == 2)
   assert(hash(4) == 1)

   -- 0 1 2) (3 4
   -- 0 4 3) (2 1
   -- 0 1 2) (3 4
   hash = compose(reverse(3, 5, n),
                  reverse(3, 5, n))
   assert(hash(0) == 0)
   assert(hash(1) == 1)
   assert(hash(2) == 2)
   assert(hash(3) == 3)
   assert(hash(4) == 4)

   -- (0 1 2) 3 4
   -- (2 1 0) 3 4
   -- 2 (1 0 3) 4
   -- 2 (3 0 1) 4
   hash = compose(reverse(0, 3, n),
                  reverse(1, 3, n))
   assert(hash(0) == 2)
   assert(hash(1) == 3)
   assert(hash(2) == 0)
   assert(hash(3) == 1)
   assert(hash(4) == 4)

   -- 0 (1 2 3) 4
   -- 0 (3 2 1) 4
   -- (0 3 2) 1 4
   -- (2 3 0) 1 4
   hash = compose(reverse(1, 3, n),
                  reverse(0, 3, n))
   assert(hash(0) == 2)
   assert(hash(1) == 3)
   assert(hash(2) == 0)
   assert(hash(3) == 1)
   assert(hash(4) == 4)

   -- (0 1 2) 3 4
   -- (2 1 0) 3 4
   -- 2 1) 0 (3 4
   -- 4 3) 0 (1 2
   hash = compose(reverse(3, 4, n),
                  reverse(0, 3, n))
   assert(hash(0) == 4)
   assert(hash(1) == 3)
   assert(hash(2) == 0)
   assert(hash(3) == 1)
   assert(hash(4) == 2)
end

local lengths = nil
local n = tonumber(os.getenv('N')) or 256
if puzzle.part == 1 then
   lengths = {}
   for len in string.gmatch(io.read(), '(%d+)') do
      len = tonumber(len)
      assert(len and len <= n)
      table.insert(lengths, len)
   end
elseif puzzle.part == 2 then
   local line = io.read() .. '\017\031\073\047\023'
   lengths = table.pack(string.byte(line, 1, #line))
end

local pos = 0
local skip = 0
local hash = function(i)
   assert(i >= 0 and i < n)
   return i
end

for round = 1, (puzzle.part == 2 and 64 or 1) do
   for _, len in ipairs(lengths) do
      hash = compose(reverse(pos, len, n), hash)
      pos = (pos + len + skip) % n
      skip = skip + 1
   end
end

if puzzle.part == 1 then
   print(hash(0) * hash(1))
elseif puzzle.part == 2 then
   local function dense(hash)
      local numbers = {}
      for block = 0, 16 - 1 do
         local acc = 0
         for number = 0, 16 - 1 do
            acc = acc ~ hash(block * 16 + number)
         end
         table.insert(numbers, acc)
      end
      return numbers
   end
   -- TODO
   print(table.unpack(dense(hash)))
end
