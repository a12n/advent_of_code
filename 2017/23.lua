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
   -- Manual solution. Analyzed the intended program from the puzzle
   -- input assembly code. Let the code in the puzzle input setup the
   -- parameters, run [optimized] Lua version of the program.
   registers['a'] = 1
   instrs[9] = { 'jnz', 1, #instrs + 1 }
   runinstrs(instrs, 1, registers)
   local n = registers['b']
   local limit = registers['c']
   local composite = 0
   while n <= limit do
      if not isprime(n) then
         composite = composite + 1
      end
      n = n + 17
   end
   print(composite)
end
