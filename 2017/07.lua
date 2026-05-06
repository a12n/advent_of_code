local parents = {}
local weights = {}
local children = {}

local root = nil

for line in io.lines() do
   local name, weight = string.match(line, '(%a+)%s*%((%d+)%)')

   assert(name and weight)
   weights[name] = tonumber(weight)
   children[name] = {}

   for child in string.gmatch(string.match(line, '->%s*(.+)$') or '', '%a+') do
      parents[child] = name
      table.insert(children[name], child)
   end

   if not root then
      root = name
   end
end

while parents[root] do
   root = parents[root]
end

if puzzle.part == 1 then
   print(root)
   return
end

local function argdiff(xs)
   for i, xi in ipairs(xs) do
      local xprev = i > 1 and xs[i - 1] or xs[#xs]
      local xnext = i < #xs and xs[i + 1] or xs[1]
      if (xi - xprev) ~= 0 and (xnext - xi) ~= 0 then
         return i, (xnext - xi)
      end
   end
end

function unbalanced(name, depth)
   local total_weight = weights[name]
   local child_total_weights = {}
   local child_total_weights_num = 0

   local unbalanced_node = nil
   local unbalanced_diff = nil

   for i, child in ipairs(children[name]) do
      local child_total_weight, child_node, child_diff = unbalanced(child, depth + 1)

      child_total_weights[i] = child_total_weight
      total_weight = total_weight + child_total_weight

      if child_node and child_diff then
         assert(not unbalanced_node and not unbalanced_diff)
         unbalanced_node = child_node
         unbalanced_diff = child_diff
      end
   end

   if not unbalanced_node or not unbalanced_diff then
      local index, diff = argdiff(child_total_weights)
      if index then
         unbalanced_node = children[name][index]
         unbalanced_diff = diff
      end
   end

   return total_weight, unbalanced_node, unbalanced_diff
end

local _, name, diff = unbalanced(root, 0)
print(weights[name] + diff)
