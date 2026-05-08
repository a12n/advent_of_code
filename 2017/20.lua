require('grid-spatial')

local function parse(line, what)
   local x, y, z = string.match(line, what .. '%s*=%s*<%s*([%d-]+)%s*,%s*([%d-]+)%s*,%s*([%d-]+)%s*>')
   if x and y and z then
      return tonumber(x), tonumber(y), tonumber(z)
   end
   return nil
end

local particles = {}
for line in io.lines() do
   table.insert(particles, { p = point.new(parse(line, 'p')),
                             v = vector.new(parse(line, 'v')),
                             a = vector.new(parse(line, 'a')) })
end

local closest = nil
local closestnorm = nil
for i, particle in ipairs(particles) do
   local norm = particle.a:taxicab()
   if not closestnorm or norm < closestnorm then
      closest = i
      closestnorm = n
   end
end
print(closest - 1)
