local steps = tonumber(io.read())
local pos = 1

if puzzle.part == 1 then
   local buffer = { 0 }
   for i = 1, 2017 do
      pos = (((pos - 1) + steps) % #buffer) + 1 + 1
      table.insert(buffer, pos, i)
   end
   print(buffer[pos + 1])
elseif puzzle.part == 2 then
   local after0 = nil
   local len = 1
   for i = 1, 50000000 do
      pos = (((pos - 1) + steps) % len) + 1 + 1
      len = len + 1
      if pos == (1 + 1) then
         after0 = i
      end
   end
   print(after0)
end
