local pipes = {}

function addpipe(from, to)
   if not pipes[from] then
      pipes[from] = {}
   end
   if not pipes[to] then
      pipes[to] = {}
   end
   table.insert(pipes[from], to)
   table.insert(pipes[to], from)
end

for line in io.lines() do
   local from = string.match(line, '^(%d+)%s*')
   from = tonumber(from)
   assert(from)
   for to in string.gmatch(string.match(line, '<->%s*([%d%s,]+)$'), '%d+') do
      to = tonumber(to)
      addpipe(from, to)
   end
end

function connected(pipes, from, id, mapping)
   id = id or 1
   mapping = mapping or {}
   if mapping[from] then
      return 0
   end
   local n = 1
   mapping[from] = id
   for _, to in ipairs(pipes[from]) do
      n = n + connected(pipes, to, id, mapping)
   end
   return n
end

function components(pipes)
   local id = 1
   local mapping = {}
   for prog, adjacent in pairs(pipes) do
      if connected(pipes, prog, id, mapping) ~= 0 then
         id = id + 1
      end
   end
   return mapping
end

if puzzle.part == 1 then
   print(connected(pipes, 0))
elseif puzzle.part == 2 then
   local groups = 0
   for _, group in pairs(components(pipes)) do
      groups = math.max(groups, group)
   end
   print(groups)
end
