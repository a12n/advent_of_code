local layers = 0
local scanners = {}

for line in io.lines() do
   local depth, range = string.match(line, '^(%d+)%s*:%s*(%d+)$')
   depth = tonumber(depth)
   range = tonumber(range)
   assert(depth and range)
   layers = math.max(layers, depth + 1)
   scanners[depth] = {
      pos   = 0,
      speed = 1,
      range = range,
   }
end

function advancescanner(pos, speed, range)
   if pos == 0 and speed == -1 then
      speed = -speed
   elseif pos == (range - 1) and speed == 1 then
      speed = -speed
   end
   return pos + speed, speed
end

local packet = 0
local severity = 0
while packet < layers do
   for depth, scanner in pairs(scanners) do
      if depth == packet and scanner.pos == 0 then
         severity = severity + packet * scanners[packet].range
      end
      scanner.pos, scanner.speed = advancescanner(scanner.pos, scanner.speed, scanner.range)
   end
   packet = packet + 1
end
print(severity)
