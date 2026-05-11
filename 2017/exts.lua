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
