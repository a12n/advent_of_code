point = {}
point.__index = point

function point.new(x, y)
   return setmetatable({ x = x or 0, y = y or x or 0 }, point)
end

function point.__add(p, u)
   return point.new(p.x + u.x, p.y + u.y)
end

function point.__tostring(p)
   return p.x .. ',' .. p.y
end

vector = {}
vector.__index = vector

function vector.new(x, y)
   return setmetatable({ x = x or 0, y = y or x or 0 }, vector)
end

function vector.__mul(u, n)
   return vector.new(u.x * n, u.y * n)
end

function vector.__tostring(u)
   return u.x .. ',' .. u.y
end

function vector.cw(u)
   return vector.new(-u.y, u.x)
end

function vector.ccw(u)
   return vector.new(u.y, -u.x)
end

function vector.taxicab(u)
   return math.abs(u.x) + math.abs(u.y)
end
