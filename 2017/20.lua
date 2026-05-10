require('grid-spatial')

local function parsepart(line, part)
   local x, y, z = string.match(line, part .. '%s*=%s*<%s*([%d-]+)%s*,%s*([%d-]+)%s*,%s*([%d-]+)%s*>')
   if x and y and z then
      return tonumber(x), tonumber(y), tonumber(z)
   end
   return nil
end

local function parseparticle(line)
   local s = vector.new(parsepart(line, 'p'))
   local v = vector.new(parsepart(line, 'v'))
   local a = vector.new(parsepart(line, 'a'))
   return s and v and a and { s = s, v = v, a = a }
end

local particles = {}
for line in io.lines() do
   table.insert(particles, assert(parseparticle(line)))
end

local closest = nil
local closestnorm = nil
for i, particle in ipairs(particles) do
   local norm = particle.a:taxicab()
   if not closestnorm or norm < closestnorm then
      closest = i
      closestnorm = norm
   end
end
print(closest - 1)
