require('cpu')

local instrs = {}
for line in io.lines() do
   table.insert(instrs, table.pack(parseinstr(line)))
end

local registers = {}
if puzzle.part == 1 then
   local counters = {}
   runinstrs(instrs, 1, registers, nil, counters)
   print(counters['mul'] or 0)
elseif puzzle.part == 2 then
   -- Manual solution. Have disassembled the intended program from the
   -- puzzle input.
   local function isprime(n)
      local d = 2
      while d * d <= n do
         if n % d == 0 then
            return false
         end
         d = d + 1
      end
      return true
   end
   local n = 106700
   local limit = 123700
   local composite = 0
   while n <= limit do
      if not isprime(n) then
         composite = composite + 1
      end
      n = n + 17
   end
   print(composite)
end
