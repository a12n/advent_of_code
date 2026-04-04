local function argmax(list)
   local i = nil
   for j = 1, #list do
      if not i or list[i] < list[j] then
         i = j
      end
   end
   -- print('argmax', 'i', i)
   -- print(table.unpack(list))
   return i
end

local function redistribute(banks, i)
   local n = banks[i]
   local j = (((i - 1) + 1) % #banks) + 1
   banks[i] = 0
   while n > 0 do
      banks[j] = banks[j] + 1
      n = n - 1
      j = ((j + 1 - 1) % #banks) + 1
   end
   -- print('redistribute', 'banks')
   -- print(table.unpack(banks))
end

local function key(banks)
   return string.char(table.unpack(banks))
end

local banks = {}
for _ = 1, tonumber(os.getenv('BANKS')) or 16 do
   table.insert(banks, assert(io.read('n')))
end

local seen = {}
local steps = 0
while not seen[key(banks)] do
   seen[key(banks)] = true
   redistribute(banks, argmax(banks))
   steps = steps + 1
end

print(steps)
