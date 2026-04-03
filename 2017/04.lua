local nvalid = 0

local function valid(passphrase)
   local seen = {}
   for word in string.gmatch(passphrase, '(%w+)') do
      if seen[word] then
         return false
      end
      seen[word] = true
   end
   return true
end

for passphrase in io.lines() do
   if valid(passphrase) then
      nvalid = nvalid + 1
   end
end

print(nvalid)
