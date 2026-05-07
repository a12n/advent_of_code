local function find(list, a)
   for i = 1, #list do
      if list[i] == a then
         return i
      end
   end
   return nil
end

local function spin(list, n)
   local tmp = {}
   table.move(list, #list - n + 1, #list, 1, tmp)
   table.move(list, 1, #list - n, 1 + n)
   table.move(tmp, 1, n, 1, list)
end

local function exchange(list, i, j)
   local a = list[i]
   list[i] = list[j]
   list[j] = a
end

local function partner(list, a, b)
   exchange(list, find(list, a), find(list, b))
end

local function parsemove(s)
   local a, b
   a = string.match(s, 's(%d+)')
   if a then
      return 'spin', tonumber(a)
   end
   a, b = string.match(s, 'x(%d+)/(%d+)')
   if a and b then
      return 'exchange', tonumber(a) + 1, tonumber(b) + 1
   end
   a, b = string.match(s, 'p(%l)/(%l)')
   if a and b then
      return 'partner', string.byte(a), string.byte(b)
   end
   return nil
end

local programs = table.pack(string.byte('abcdefghijklmnop', 1, tonumber(os.getenv('N')) or 16))

-- print('programs', string.char(table.unpack(programs)))
for token in string.gmatch(io.read(), '([^%s,]+)') do
   local move, a, b = parsemove(token)
   if move == 'spin' then
      spin(programs, a)
   elseif move == 'exchange' then
      exchange(programs, a, b)
   elseif move == 'partner' then
      partner(programs, a, b)
   end
   -- ({ ['spin'] = spin, ['exchange'] = exchange, ['partner'] = partner })[move](programs, a, b)
   -- moves[move](a, b)
   -- print('move', move, a, b)
end
print(string.char(table.unpack(programs)))
