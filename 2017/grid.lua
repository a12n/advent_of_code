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

-- Returns sequence table of `k` (must be 4 or 8 for Von Neumann or
-- Moore) neighborhood of the given point `p`.
function point.neighbors(p, k)
   local qs = {}
   for _, u in ipairs(vector.neighbors(k)) do
      table.insert(qs, p + u)
   end
   return qs
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

function vector.neighbors(k)
   if (k or 4) == 4 then
      return {
         vector.new(0, -1),
         vector.new(-1, 0), vector.new(1, 0),
         vector.new(0, 1),
      }
   elseif k == 8 then
      return {
         vector.new(-1, -1), vector.new(0, -1), vector.new(1, -1),
         vector.new(-1, 0), vector.new(1, 0),
         vector.new(-1, 1), vector.new(0, 1), vector.new(1, 1),
      }
   else
      error('invalid vector neighborhood')
   end
end

grid = {}
grid.__index = grid

function grid.new(linesstr)
   local g = {}
   local ncols = nil
   for line in string.gmatch(linesstr or '', '[^\n]+') do
      local row = table.pack(string.byte(line, 1, #line))
      if not ncols then
         ncols = #row
      end
      if #row ~= ncols then
         error('row length mismatch')
      end
      table.insert(g, row)
   end
   return setmetatable(g, grid)
end

function grid.nrows(g)
   return #g
end

function grid.ncols(g)
   local row = rawget(g, 1)
   return row and #row or 0
end

function grid.get(g, p)
   local row = rawget(g, p.y)
   return row and rawget(row, p.x) or nil
end

function grid.set(g, p, v)
   local row = rawget(g, p.y)
   if not row then
      row = {}
      rawset(g, p.y, row)
   end
   rawset(row, p.x, v)
end
