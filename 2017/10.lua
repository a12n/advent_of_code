local function reverse(from, len, n)
   local to = from + len
   local tomod = to % n
   if to <= n then
      -- from=2 len=2 n=5
      --      f    t
      -- 0 1 (2 3) 4
      -- 0 1 (3 2) 4
      return function(i)
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
         if i >= from then
            return (tomod - 1) - (i - from)
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
end

local n = tonumber(os.getenv('N')) or 256

if puzzle.part == 1 then
   local pos = 0
   local skip = 0
   local hash = function(i)
      assert(i >= 0 and i < n)
      return i
   end

   local function reconstruct(hash, n)
      local list = {}
      for i = 1, n do
         table.insert(list, hash(i - 1))
      end
      return list
   end

   for len in string.gmatch(io.read(), '(%d+)') do
      len = tonumber(len)
      print('before')
      print('pos', pos, 'skip', skip, 'len', len)
      assert(len <= n)
      print('list', table.unpack(reconstruct(hash, n)))
      hash = compose(hash, reverse(pos, len, n))
      pos = (pos + len + skip) % n
      skip = skip + 1
      print('after')
      print('pos', pos, 'skip', skip)
      print('list', table.unpack(reconstruct(hash, n)))
   end

   print(hash(0) * hash(1))
end
