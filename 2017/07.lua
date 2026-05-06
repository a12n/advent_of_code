local nodechildren = {}
local nodeparent = {}
local nodeweight = {}
local root = nil

for line in io.lines() do
   local node, weight = string.match(line, '(%a+)%s*%((%d+)%)')
   assert(node and weight)
   nodeweight[node] = tonumber(weight)
   nodechildren[node] = {}
   for cnode in string.gmatch(string.match(line, '->%s*(.+)$') or '', '%a+') do
      nodeparent[cnode] = node
      table.insert(nodechildren[node], cnode)
   end
   if not root then
      root = node
   end
end

while nodeparent[root] do
   root = nodeparent[root]
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

function unbalanced(node)
   local weight = nodeweight[node]
   local cweights = {}

   local unode = nil
   local udiff = nil

   for i, cnode in ipairs(nodechildren[node]) do
      local cunode, cudiff, cweight = unbalanced(cnode)

      cweights[i] = cweight
      weight = weight + cweight

      if cunode and cudiff then
         assert(not unode and not udiff)
         unode = cunode
         udiff = cudiff
      end
   end

   if not unode or not udiff then
      local i = nil
      i, udiff = argdiff(cweights)
      if i then
         unode = nodechildren[node][i]
      end
   end

   return unode, udiff, weight
end

local node, diff = unbalanced(root)
print(nodeweight[node] + diff)
