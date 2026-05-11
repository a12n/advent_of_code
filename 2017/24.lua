local function lesspins(comp)
   return comp // 100
end

local function morepins(comp)
   return comp % 100
end

local function strength(comp)
   return lesspins(comp) + morepins(comp)
end

local function limits(components)
   local len = 0
   local stren = 0
   for comp, avail in pairs(components) do
      if avail then
         len = len + 1
         stren = stren + strength(comp)
      end
   end
   return len, stren
end

local function parsecomponent(s)
   local less, more = string.match(s, '%s*(%d+)%s*/%s*(%d+)%s*')
   if not less or not more then
      return nil
   end
   less = tonumber(less)
   more = tonumber(more)
   if less > 99 or more > 99 then
      return nil
   end
   if less > more then
      less, more = more, less
   end
   return less * 100 + more
end

local components = {}
for line in io.lines() do
   components[assert(parsecomponent(line))] = true
end

local maxlen = nil
local maxstren = nil

local function search(port, len, stren, lenavail, strenavail, components)
   if lenavail == 0 or strenavail == 0 then
      return
   end

   if puzzle.part == 1 then
      if maxstren and maxstren >= (stren + strenavail) then
         return
      end
      if not maxstren or stren > maxstren then
         maxstren = stren
      end
   elseif puzzle.part == 2 then
      if maxlen and maxlen > (len + lenavail) then
         return
      end
      if not maxlen then
         maxlen = len
         maxstren = stren
      elseif len > maxlen then
         maxlen = len
         maxstren = stren
      elseif len == maxlen and stren > maxstren then
         maxstren = stren
      end
   end

   for comp, avail in pairs(components) do
      if avail then
         if lesspins(comp) == port then
            components[comp] = false
            search(morepins(comp), len + 1, stren + strength(comp), lenavail - 1, strenavail - strength(comp), components)
            components[comp] = true
         elseif morepins(comp) == port then
            components[comp] = false
            search(lesspins(comp), len + 1, stren + strength(comp), lenavail - 1, strenavail - strength(comp), components)
            components[comp] = true
         end
      end
   end
end

do
   local lenavail, strenavail = limits(components)
   search(0, 0, 0, lenavail, strenavail, components)
end
print(maxstren)
