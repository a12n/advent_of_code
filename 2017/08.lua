local regs = {}

local modifiers = {
   ['inc'] = function(r, n) regs[r] = (regs[r] or 0) + n end,
   ['dec'] = function(r, n) regs[r] = (regs[r] or 0) - n end,
}

local conds = {
   ['<'] = function(r, n) return (regs[r] or 0) < n end,
   ['>'] = function(r, n) return (regs[r] or 0) > n end,
   ['<='] = function(r, n) return (regs[r] or 0) <= n end,
   ['>='] = function(r, n) return (regs[r] or 0) >= n end,
   ['=='] = function(r, n) return (regs[r] or 0) == n end,
   ['!='] = function(r, n) return (regs[r] or 0) ~= n end,
}

local instrs = {}
local max_during = nil

for line in io.lines() do
   local r, op, n, cr, cop, cn =
      string.match(line, '^(%w+)%s+(%w+)%s+(-?%d+)%s+if%s+(%w+)%s*(%p+)%s*(-?%d+)$')
   assert(r and op and n and cr and cop and cn)
   n = tonumber(n)
   cn = tonumber(cn)
   local cond = conds[cop]
   local modifier = modifiers[op]
   assert(cond and modifier)
   local instr = function()
      if cond(cr, cn) then
         modifier(r, n)
         if not max_during or (regs[r] or 0) > max_during then
            max_during = regs[r]
         end
      end
   end
   table.insert(instrs, instr)
end

for i = 1, #instrs do
   instrs[i]()
end

if puzzle.part == 1 then
   local max_after = nil
   for r, n in pairs(regs) do
      if not max_after or n > max_after then
         max_after = n
      end
   end
   print(max_after)
elseif puzzle.part == 2 then
   print(max_during)
end
