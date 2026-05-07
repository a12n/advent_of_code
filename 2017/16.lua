local nprograms = tonumber(os.getenv('N')) or 16

local function parsemove(s)
   local a, b
   a = string.match(s, 's(%d+)')
   if a then
      return 'spin', tonumber(a)
   end
   a, b = string.match(s, 'x(%d+)/(%d+)')
   if a and b then
      return 'exchange', tonumber(a), tonumber(b)
   end
   a, b = string.match(s, 'p(%l)/(%l)')
   if a and b then
      return 'partner', a, b
   end
   return nil
end

for token in string.gmatch(io.read(), '([^%s,]+)') do
   print(parsemove(token))
end
