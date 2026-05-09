require('knothash')

local n = tonumber(os.getenv('N')) or 256

if puzzle.part == 1 then
   local bytes = {}
   for len in string.gmatch(io.read(), '(%d+)') do
      len = tonumber(len)
      assert(len and len <= n)
      table.insert(bytes, len)
   end
   local hash = knothash(string.char(table.unpack(bytes)), n, 1, '')
   print(hash(0) * hash(1))
elseif puzzle.part == 2 then
   print(formatdense(densehash(knothash(io.read()))))
end
