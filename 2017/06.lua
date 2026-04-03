local banks = {}
for _ = 1, tonumber(os.getenv('BANKS')) or 16 do
   table.insert(banks, assert(io.read('n')))
end

-- TODO
print(table.unpack(banks))
