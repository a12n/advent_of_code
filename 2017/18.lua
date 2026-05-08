local played = nil
local registers = {}
local instructions = {}
local ip = 1
local halt = false

local function parseinstr(s)
   local x, y, n, m, o

   o, x, y = string.match(s, '^(%a+)%s+([%a%d-]+)%s+([%a%d-]+)$')
   n = tonumber(x)
   m = tonumber(y)
   if o and x and y then
      if o == 'set' then
         assert(not n)
         return function()
            registers[x] = (m or registers[y] or 0)
            return 1
         end
      elseif o == 'add' then
         assert(not n)
         return function()
            registers[x] = (registers[x] or 0) + (m or registers[y] or 0)
            return 1
         end
      elseif o == 'mul' then
         assert(not n)
         return function()
            registers[x] = (registers[x] or 0) * (m or registers[y] or 0)
            return 1
         end
      elseif o == 'mod' then
         assert(not n)
         return function()
            registers[x] = (registers[x] or 0) % (m or registers[y] or 0)
            return 1
         end
      elseif o == 'jgz' then
         return function()
            if (n or registers[x] or 0) > 0 then
               return (m or assert(registers[y]))
            end
            return 1
         end
      end
   end

   o, x = string.match(s, '^(%a+)%s+([%a%d-]+)$')
   n = tonumber(x)
   if o and x then
      if o == 'snd' then
         return function()
            played = n or registers[x] or 0
            return 1
         end
      elseif o == 'rcv' then
         return function()
            if (n or registers[x] or 0) ~= 0 then
               print(played)
               return #instructions
            end
            return 1
         end
      end
   end

   return nil
end

for line in io.lines() do
   table.insert(instructions, assert(parseinstr(line)))
end

while ip >= 1 and ip <= #instructions do
   ip = ip + instructions[ip]()
end
