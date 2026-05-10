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

local function updateparticle(p)
   p.v = p.v + p.a
   p.s = p.s + p.v
end

local particles = {}
for line in io.lines() do
   table.insert(particles, assert(parseparticle(line)))
end

if puzzle.part == 1 then
   local index = nil
   local minanorm = nil
   for i, p in ipairs(particles) do
      -- FIXME: There may be multiple particles with accel norm.
      local anorm = p.a:taxicab()
      if not minanorm or anorm < minanorm then
         index = i
         minanorm = anorm
      end
   end
   print(index - 1)
elseif puzzle.part == 2 then
   local n = #particles
   for t = 1, 1000 do
      -- Collision
      for i = 1, n - 1 do
         local p = particles[i]
         if p then
            for j = i + 1, n do
               local q = particles[j]
               if q then
                  if p.s == q.s then
                     particles[i] = nil
                     particles[j] = nil
                  end
               end
            end
         end
      end
      -- Update
      for i = 1, n do
         local p = particles[i]
         if p then
            updateparticle(p)
         end
      end
   end
   -- Count loop
   local m = 0
   for _, p in pairs(particles) do
      m = m + 1
   end
   print(m)
end
