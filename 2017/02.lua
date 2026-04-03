local spreadsheet = {}

for line in io.lines() do
   local row = {}
   for value in string.gmatch(line, '(%d+)%s*') do
      table.insert(row, tonumber(value))
   end
   table.insert(spreadsheet, row)
end

local rowchecksum =
   (puzzle.part == 1 and function(row)
       local min, max
       for _, value in ipairs(row) do
          min = math.min(value, min or value)
          max = math.max(value, max or value)
       end
       return max - min
   end) or
   (puzzle.part == 2 and function(row)
       for i = 1, #row - 1 do
          for j = i + 1, #row do
             local n = math.max(row[i], row[j])
             local m = math.min(row[i], row[j])
             if n % m == 0 then
                return n // m
             end
          end
       end
       error('no solution')
   end)

local checksum = 0
for _, row in ipairs(spreadsheet) do
   checksum = checksum + rowchecksum(row)
end
print(checksum)
