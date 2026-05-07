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

function investigate(pipes, from, visited)
   if not visited then
      visited = {}
   end

   if visited[from] then
      return 0
   end

   local n = 1
   visited[from] = true

   for _, to in pairs(pipes[from]) do
      n = n + investigate(pipes, to, visited)
   end

   return n
end

print(investigate(pipes, 0))
