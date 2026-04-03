local normalize =
   (puzzle.part == 1 and function(passphrase) return
          passphrase
   end) or
   (puzzle.part == 2 and function(passphrase)
       local bytes = { string.byte(passphrase, 1, #passphrase) }
       table.sort(bytes)
       return string.char(table.unpack(bytes))
   end)

local function valid(passphrase)
   local seen = {}
   for word in string.gmatch(passphrase, '(%w+)') do
      word = normalize(word)
      if seen[word] then
         return false
      end
      seen[word] = true
   end
   return true
end

local n = 0
for passphrase in io.lines() do
   if valid(passphrase) then
      n = n + 1
   end
end
print(n)
