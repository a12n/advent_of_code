local parents = {}
local weights = {}

for line in io.lines() do
   local name, weight = string.match(line, '(%a+)%s*%((%d+)%)')
   assert(name and weight)
   weights[name] = tonumber(weight)
   for child in string.gmatch(string.match(line, '->%s*(.+)$') or '', '%a+') do
      parents[child] = name
   end
end

local root = next(weights)
while parents[root] do
   root = parents[root]
end
print(root)
