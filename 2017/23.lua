require('cpu')

local instrs = {}
for line in io.lines() do
   table.insert(instrs, table.pack(parseinstr(line)))
end
-- printinstrs(instrs)
-- print()

local registers = {}

if puzzle.part == 1 then
   local counters = {}
   runinstrs(instrs, 1, registers, nil, counters)
   print(counters['mul'] or 0)
elseif puzzle.part == 2 then
   ------ Loop 1 ------
   -- b       =       106700
   -- e       =       2
   -- g       =       -106696
   --
   -- 17      :       sub     e       -1 => -106698
   -- 18      :       set     g       e
   -- 19      :       sub     g       b
   -- 20      :       jnz     g       -8

   ------ Loop 2 ------
   -- b       =       106700
   -- d       =       2
   --
   -- 21      :       sub     d       -1 => -106698
   -- 22      :       set     g       d
   -- 23      :       sub     g       b
   -- 24      :       jnz     g       -13

   registers['a'] = 1
   instrs[20] = { 'jnz', 0, -8 }
   instrs[24] = { 'jnz', 0, -13 }
   instrs[25] = { 'jnz', 0, 2 }
   runinstrs(instrs, 1, registers)
   print(registers['h'])
end
