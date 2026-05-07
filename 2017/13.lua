for line in io.lines() do
   local depth, range = string.match(line, '^(%d+)%s*:%s*(%d+)$')
   depth = tonumber(depth)
   range = tonumber(range)
   assert(depth and range)
   print(depth, range)
end
