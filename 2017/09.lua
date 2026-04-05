-- local stack = {}
-- local groups = 0
-- local score = 0

local groups = 0
local score = 0

function group(c)
   print('group', c)
   if c == '{' then
      groups = groups + 1
      print('groups', groups)
      return group
   elseif c == '}' then
      score = score + groups
      groups = groups - 1
      print('groups', groups, 'score', score)
      return group
   elseif c == ',' then
      return group
   elseif c == '<' then
      return garbage
   else
      print('invalid input')
      return nil
   end
end

function garbage(c)
   print('garbage', c)
   if c == '>' then
      return group
   elseif c == '!' then
      return ignore
   else
      return garbage
   end
end

function ignore(c)
   print('ignore', c)
   return garbage
end

local pos = 0
local state = group
while true do
   local c = io.read(1)
   if not c then
      break
   end
   if c ~= ' ' and c ~= '\n' then
      state = state(c)
      if not state then
         error(string.format('error at position %d', pos))
      end
   end
   pos = pos + 1
end
print(score)
