require('cpu')

local instrs = {}
for line in io.lines() do
   table.insert(instrs, table.pack(parseinstr(line)))
end

local counters = {}
runinstrs(instrs, 1, {}, nil, counters)
print(counters['mul'] or 0)
