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
      return function(list)
         spin(list, tonumber(a))
      end
   end
   a, b = string.match(s, 'x(%d+)/(%d+)')
   if a and b then
      return function(list)
         exchange(list, tonumber(a) + 1, tonumber(b) + 1)
      end
   end
   a, b = string.match(s, 'p(%l)/(%l)')
   if a and b then
      return function(list)
         partner(list, string.byte(a), string.byte(b))
      end
   end
   return nil
end

local function dance(programs, moves)
   for _, move in ipairs(moves) do
      move(programs)
   end
end

local function inorder(programs)
   local pj = string.byte('a')
   for _, pi in ipairs(programs) do
      if pi ~= pj then
         return false
      end
      pj = pj + 1
   end
   return true
end

local moves = {}
local programs = table.pack(string.byte('abcdefghijklmnop', 1, tonumber(os.getenv('N')) or 16))

for token in string.gmatch(io.read(), '([^%s,]+)') do
   table.insert(moves, assert(parsemove(token)))
end

if puzzle.part == 1 then
   dance(programs, moves)
elseif puzzle.part == 2 then
   local limit = 1000000000
   local times = limit
   while times > 0 do
      dance(programs, moves)
      times = times - 1
      if inorder(programs) then
         times = times % (limit - times)
      end
   end
end

print(string.char(table.unpack(programs)))
