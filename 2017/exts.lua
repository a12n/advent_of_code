function table.find(list, item)
   for i = 1, #list do
      if list[i] == item then
         return i
      end
   end
   return nil
end

function table.map(list, f)
   local ans = {}
   for _, v in ipairs(list) do
      table.insert(ans, f(v, k))
   end
   return ans
end

function table.reduce(list, f, acc)
   for _, v in ipairs(list) do
      acc = f(acc, v, k)
   end
   return acc
end

-- Operators as addressable functions.
function math.add(a, b) return a + b end
function math.sub(a, b) return a - b end
function math.mul(a, b) return a * b end
function math.div(a, b) return a / b end

function math.sum(...)
   return table.reduce({...}, math.add, 0)
end

function math.prod(...)
   return table.reduce({...}, math.mul, 1)
end
