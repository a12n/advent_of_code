point = {}
point.__index = point

function point.new(x, y, z)
   return setmetatable({ x = x or 0,
                         y = y or x or 0,
                         z = z or y or x or 0 }, point)
end

function point.__eq(p, q)
   return p.x == q.x and p.y == q.y and p.z == q.z
end

function point.__add(p, u)
   return point.new(p.x + u.x, p.y + u.y, p.z + u.z)
end

function point.__tostring(p)
   return p.x .. ',' .. p.y .. ',' .. p.z
end

vector = {}
vector.__index = vector

function vector.new(x, y, z)
   return setmetatable({ x = x or 0,
                         y = y or x or 0,
                         z = z or y or x or 0 }, vector)
end

function vector.__eq(u, v)
   return u.x == v.x and u.y == v.y and u.z == v.z
end

function vector.__add(u, v)
   return vector.new(u.x + v.x, u.y + v.y, u.z + v.z)
end

function vector.__mul(u, n)
   return vector.new(u.x * n, u.y * n, u.z * n)
end

function vector.__div(u, n)
   return vector.new(u.x / n, u.y / n, u.z / n)
end

function vector.__idiv(u, n)
   return vector.new(u.x // n, u.y // n, u.z // n)
end

function vector.__tostring(u)
   return u.x .. ',' .. u.y .. ',' .. u.z
end

function vector.taxicab(u)
   return math.abs(u.x) + math.abs(u.y) + math.abs(u.z)
end
