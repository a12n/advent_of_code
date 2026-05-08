function table.find(list, item)
   for i = 1, #list do
      if list[i] == item then
         return i
      end
   end
   return nil
end
