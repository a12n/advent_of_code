local regs = {}
local prog = {}

local actions = {
   ['inc'] = function(r, n) regs[r] = (regs[r] or 0) + n end,
   ['dec'] = function(r, n) regs[r] = (regs[r] or 0) - n end,
}

local cops = {
   ['<'] = function(r, n) return (regs[r] or 0) < n end,
   ['>'] = function(r, n) return (regs[r] or 0) > n end,
   ['<='] = function(r, n) return (regs[r] or 0) <= n end,
   ['>='] = function(r, n) return (regs[r] or 0) >= n end,
   ['=='] = function(r, n) return (regs[r] or 0) == n end,
   ['!='] = function(r, n) return (regs[r] or 0) ~= n end,
}

local highest_value = nil
for line in io.lines() do
   local reg, op, n, creg, cop, cn = string.match(line, '^(%w+)%s+(%w+)%s+(-?%d+)%s+if%s+(%w+)%s*(%p+)%s*(-?%d+)$')
   assert(reg and op and n and creg and cop and cn)
   n = tonumber(n)
   cn = tonumber(cn)
   print(reg, op, n, creg, cop, cn)
   local condf = cops[cop]
   local actionf = actions[op]
   assert(condf and actionf)
   table.insert(prog,
                function()
                   if condf(creg, cn) then
                      actionf(reg, n)
                      if not highest_value or (regs[reg] or 0) > highest_value then
                         highest_value = (regs[reg] or 0)
                      end
                   end
                end)
end

for i = 1, #prog do
   prog[i]()
end

if puzzle.part == 1 then
   local max = nil
   for r, n in pairs(regs) do
      if not max or n > max then
         max = n
      end
   end
   print(max)
elseif puzzle.part == 2 then
   print(highest_value)
end
