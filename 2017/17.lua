local steps = tonumber(io.read())
local buffer = { 0 }
local pos = 1

for i = 1, 2017 do
   pos = (((pos - 1) + steps) % #buffer) + 1 + 1
   table.insert(buffer, pos, i)
end
print(buffer[pos + 1])
